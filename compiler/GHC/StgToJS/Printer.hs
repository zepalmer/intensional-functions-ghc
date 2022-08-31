{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.StgToJS.Printer
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Jeffrey Young  <jeffrey.young@iohk.io>
--                Luite Stegeman <luite.stegeman@iohk.io>
--                Sylvain Henry  <sylvain.henry@iohk.io>
-- Stability   :  experimental
--
-- Custom prettyprinter for JS AST uses the JS PPr module for most of
-- the work
--
--
-----------------------------------------------------------------------------
module GHC.StgToJS.Printer
  ( pretty
  , ghcjsRenderJs
  , prettyBlock
  )
where

import GHC.Prelude


import GHC.JS.Syntax
import GHC.JS.Ppr

import GHC.Utils.Ppr      as PP
import GHC.Data.FastString
import GHC.Types.Unique.Map

import Data.List (sortOn)
import Data.Char (isAlpha,isDigit)

pretty :: JStat -> Doc
pretty = jsToDocR ghcjsRenderJs

ghcjsRenderJs :: RenderJs
ghcjsRenderJs = defaultRenderJs { renderJsV = ghcjsRenderJsV
                                , renderJsS = ghcjsRenderJsS
                                }

-- attempt to resugar some of the common constructs
ghcjsRenderJsS :: RenderJs -> JStat -> Doc
ghcjsRenderJsS r (BlockStat xs) = prettyBlock r (flattenBlocks xs)
ghcjsRenderJsS r s              = renderJsS defaultRenderJs r s

-- don't quote keys in our object literals, so closure compiler works
ghcjsRenderJsV :: RenderJs -> JVal -> Doc
ghcjsRenderJsV r (JHash m)
  | isNullUniqMap m = text "{}"
  | otherwise       = braceNest . PP.fsep . punctuate comma .
                          map (\(x,y) -> quoteIfRequired x <> PP.colon <+> jsToDocR r y)
                          -- nonDetEltsUniqMap doesn't introduce non-determinism here because
                          -- we sort the elements lexically
                          . sortOn (LexicalFastString . fst) $ nonDetEltsUniqMap m
  where
    quoteIfRequired :: FastString -> Doc
    quoteIfRequired x
      | isUnquotedKey x' = text x'
      | otherwise        = PP.squotes (text x')
      where x' = unpackFS x

    isUnquotedKey :: String -> Bool
    isUnquotedKey x | null x        = False
                    | all isDigit x = True
                    | otherwise     = validFirstIdent (head x)
                                      && all validOtherIdent (tail x)


    validFirstIdent c = c == '_' || c == '$' || isAlpha c
    validOtherIdent c = isAlpha c || isDigit c
ghcjsRenderJsV r v = renderJsV defaultRenderJs r v

prettyBlock :: RenderJs -> [JStat] -> Doc
prettyBlock r xs = vcat $ map addSemi (prettyBlock' r xs)

-- recognize common patterns in a block and convert them to more idiomatic/concise javascript
prettyBlock' :: RenderJs -> [JStat] -> [Doc]
-- resugar for loops with/without var declaration
prettyBlock' r ( (DeclStat i)
              : (AssignStat (ValExpr (JVar i')) v0)
              : (WhileStat False p (BlockStat bs))
              : xs
              )
     | i == i' && not (null flat) && isForUpdStat (last flat)
     = mkFor r True i v0 p (last flat) (init flat) : prettyBlock' r xs
        where
          flat = flattenBlocks bs
prettyBlock' r ( (AssignStat (ValExpr (JVar i)) v0)
               : (WhileStat False p (BlockStat bs))
               : xs
               )
     | not (null flat) && isForUpdStat (last flat)
     = mkFor r False i v0 p (last flat) (init flat) : prettyBlock' r xs
        where
          flat = flattenBlocks bs

-- global function (does not preserve semantics but works for GHCJS)
prettyBlock' r ( (DeclStat i)
               : (AssignStat (ValExpr (JVar i')) (ValExpr (JFunc is b)))
               : xs
               )
      | i == i' = (hangBrace (text "function" <+> jsToDocR r i <> parens (fsep . punctuate comma . map (jsToDocR r) $ is))
                             (jsToDocR r b)
                  ) : prettyBlock' r xs
-- declare/assign
prettyBlock' r ( (DeclStat i)
               : (AssignStat (ValExpr (JVar i')) v)
               : xs
               )
      | i == i' = (text "var" <+> jsToDocR r i <+> char '=' <+> jsToDocR r v) : prettyBlock' r xs

-- modify/assign operators
prettyBlock' r ( (AssignStat (ValExpr (JVar i)) (InfixExpr AddOp (ValExpr (JVar i')) (ValExpr (JInt 1))))
               : xs
               )
      | i == i' = (text "++" <> jsToDocR r i) : prettyBlock' r xs
prettyBlock' r ( (AssignStat (ValExpr (JVar i)) (InfixExpr SubOp (ValExpr (JVar i')) (ValExpr (JInt 1))))
               : xs
               )
      | i == i' = (text "--" <> jsToDocR r i) : prettyBlock' r xs
prettyBlock' r ( (AssignStat (ValExpr (JVar i)) (InfixExpr AddOp (ValExpr (JVar i')) e))
               : xs
               )
      | i == i' = (jsToDocR r i <+> text "+=" <+> jsToDocR r e) : prettyBlock' r xs
prettyBlock' r ( (AssignStat (ValExpr (JVar i)) (InfixExpr SubOp (ValExpr (JVar i')) e))
               : xs
               )
      | i == i' = (jsToDocR r i <+> text "-=" <+> jsToDocR r e) : prettyBlock' r xs


prettyBlock' r (x:xs) = jsToDocR r x : prettyBlock' r xs
prettyBlock' _ [] = []

-- build the for block
mkFor :: RenderJs -> Bool -> Ident -> JExpr -> JExpr -> JStat -> [JStat] -> Doc
mkFor r decl i v0 p s1 sb = hangBrace (text "for" <> forCond)
                                      (jsToDocR r $ BlockStat sb)
    where
      c0 | decl      = text "var" <+> jsToDocR r i <+> char '=' <+> jsToDocR r v0
         | otherwise =                jsToDocR r i <+> char '=' <+> jsToDocR r v0
      forCond = parens $ hcat $ interSemi
                            [ c0
                            , jsToDocR r p
                            , parens (jsToDocR r s1)
                            ]

-- check if a statement is suitable to be converted to something in the for(;;x) position
isForUpdStat :: JStat -> Bool
isForUpdStat UOpStat {}    = True
isForUpdStat AssignStat {} = True
isForUpdStat ApplStat {}   = True
isForUpdStat _             = False

interSemi :: [Doc] -> [Doc]
interSemi [] = [PP.empty]
interSemi [s] = [s]
interSemi (x:xs) = x <> text ";" : interSemi xs

addSemi :: Doc -> Doc
addSemi x = x <> text ";"
