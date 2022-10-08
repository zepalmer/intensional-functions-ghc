{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE RequiredTypeArguments #-}

module Main where

import TestUtils
import qualified Data.Map as M
import Data.Foldable

f :: forall a -> a -> Maybe a
f (type t) (x :: t) = Just x

p1,p2 :: (Int,Int)
p1 = (11,13)
p2 = (11,28)

selectPoint' :: HieFile -> (Int,Int) -> HieAST Int
selectPoint' hf loc =
  maybe (error "point not found") id $ selectPoint hf loc

main = do
  (df, hf) <- readTestHie "HieVdq.hie"
  forM_ [p1,p2] $ \point -> do
    putStr $ "At " ++ show point ++ ", got type: "
    let types = concatMap nodeType $ getSourcedNodeInfo $ sourcedNodeInfo $ selectPoint' hf point
    forM_ types $ \typ -> do
      putStrLn (renderHieType df $ recoverFullType typ (hie_types hf))
