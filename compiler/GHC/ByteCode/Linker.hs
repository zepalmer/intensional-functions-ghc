{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -optc-DNON_POSIX_SOURCE #-}
--
--  (c) The University of Glasgow 2002-2006
--

-- | Bytecode assembler and linker
module GHC.ByteCode.Linker
  ( ClosureEnv
  , emptyClosureEnv
  , extendClosureEnv
  , linkBCO
  , lookupStaticPtr
  , lookupIE
  , nameToCLabel
  , linkFail
  )
where

import GHC.Prelude

import GHC.Runtime.Interpreter
import GHC.ByteCode.Types
import GHCi.RemoteTypes
import GHCi.ResolvedBCO
import GHCi.BreakArray

import GHC.Builtin.PrimOps

import GHC.Unit.Types

import GHC.Data.FastString
import GHC.Data.SizedSeq

import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Utils.Outputable

import GHC.Types.Name
import GHC.Types.Name.Env

import Language.Haskell.Syntax.Module.Name

-- Standard libraries
import Data.Array.Unboxed
import Foreign.Ptr
import GHC.Exts

{-
  Linking interpretables into something we can run
-}

type ClosureEnv = NameEnv (Name, ForeignHValue)

emptyClosureEnv :: ClosureEnv
emptyClosureEnv = emptyNameEnv

extendClosureEnv :: ClosureEnv -> [(Name,ForeignHValue)] -> ClosureEnv
extendClosureEnv cl_env pairs
  = extendNameEnvList cl_env [ (n, (n,v)) | (n,v) <- pairs]

{-
  Linking interpretables into something we can run
-}

linkBCO
  :: Interp
  -> ItblEnv
  -> ClosureEnv
  -> NameEnv Int
  -> RemoteRef BreakArray
  -> UnlinkedBCO
  -> IO ResolvedBCO
linkBCO interp ie ce bco_ix breakarray
           (UnlinkedBCO _ arity insns bitmap lits0 ptrs0) = do
  -- fromIntegral Word -> Word64 should be a no op if Word is Word64
  -- otherwise it will result in a cast to longlong on 32bit systems.
  lits <- mapM (fmap fromIntegral . lookupLiteral interp ie) (ssElts lits0)
  ptrs <- mapM (resolvePtr interp ie ce bco_ix breakarray) (ssElts ptrs0)
  return (ResolvedBCO isLittleEndian arity insns bitmap
              (listArray (0, fromIntegral (sizeSS lits0)-1) lits)
              (addListToSS emptySS ptrs))

lookupLiteral :: Interp -> ItblEnv -> BCONPtr -> IO Word
lookupLiteral interp ie ptr = case ptr of
  BCONPtrWord lit -> return lit
  BCONPtrLbl  sym -> do
    Ptr a# <- lookupStaticPtr interp sym
    return (W# (int2Word# (addr2Int# a#)))
  BCONPtrItbl nm -> do
    Ptr a# <- lookupIE interp ie nm
    return (W# (int2Word# (addr2Int# a#)))
  BCONPtrStr _ ->
    -- should be eliminated during assembleBCOs
    panic "lookupLiteral: BCONPtrStr"

lookupStaticPtr :: Interp -> FastString -> IO (Ptr ())
lookupStaticPtr interp addr_of_label_string = do
  m <- lookupSymbol interp addr_of_label_string
  case m of
    Just ptr -> return ptr
    Nothing  -> linkFail "GHC.ByteCode.Linker: can't find label"
                  (unpackFS addr_of_label_string)

lookupIE :: Interp -> ItblEnv -> Name -> IO (Ptr ())
lookupIE interp ie con_nm =
  case lookupNameEnv ie con_nm of
    Just (_, ItblPtr a) -> return (fromRemotePtr (castRemotePtr a))
    Nothing -> do -- try looking up in the object files.
       let sym_to_find1 = nameToCLabel con_nm "con_info"
       m <- lookupSymbol interp sym_to_find1
       case m of
          Just addr -> return addr
          Nothing
             -> do -- perhaps a nullary constructor?
                   let sym_to_find2 = nameToCLabel con_nm "static_info"
                   n <- lookupSymbol interp sym_to_find2
                   case n of
                      Just addr -> return addr
                      Nothing   -> linkFail "GHC.ByteCode.Linker.lookupIE"
                                      (unpackFS sym_to_find1 ++ " or " ++
                                       unpackFS sym_to_find2)

lookupPrimOp :: Interp -> PrimOp -> IO (RemotePtr ())
lookupPrimOp interp primop = do
  let sym_to_find = primopToCLabel primop "closure"
  m <- lookupSymbol interp (mkFastString sym_to_find)
  case m of
    Just p -> return (toRemotePtr p)
    Nothing -> linkFail "GHC.ByteCode.Linker.lookupCE(primop)" sym_to_find

resolvePtr
  :: Interp
  -> ItblEnv
  -> ClosureEnv
  -> NameEnv Int
  -> RemoteRef BreakArray
  -> BCOPtr
  -> IO ResolvedBCOPtr
resolvePtr interp ie ce bco_ix breakarray ptr = case ptr of
  BCOPtrName nm
    | Just ix <- lookupNameEnv bco_ix nm
    -> return (ResolvedBCORef ix) -- ref to another BCO in this group

    | Just (_, rhv) <- lookupNameEnv ce nm
    -> return (ResolvedBCOPtr (unsafeForeignRefToRemoteRef rhv))

    | otherwise
    -> assertPpr (isExternalName nm) (ppr nm) $
       do
          let sym_to_find = nameToCLabel nm "closure"
          m <- lookupSymbol interp sym_to_find
          case m of
            Just p -> return (ResolvedBCOStaticPtr (toRemotePtr p))
            Nothing -> linkFail "GHC.ByteCode.Linker.lookupCE" (unpackFS sym_to_find)

  BCOPtrPrimOp op
    -> ResolvedBCOStaticPtr <$> lookupPrimOp interp op

  BCOPtrBCO bco
    -> ResolvedBCOPtrBCO <$> linkBCO interp ie ce bco_ix breakarray bco

  BCOPtrBreakArray
    -> return (ResolvedBCOPtrBreakArray breakarray)

linkFail :: String -> String -> IO a
linkFail who what
   = throwGhcExceptionIO (ProgramError $
        unlines [ "",who
                , "During interactive linking, GHCi couldn't find the following symbol:"
                , ' ' : ' ' : what
                , "This may be due to you not asking GHCi to load extra object files,"
                , "archives or DLLs needed by your current session.  Restart GHCi, specifying"
                , "the missing library using the -L/path/to/object/dir and -lmissinglibname"
                , "flags, or simply by naming the relevant files on the GHCi command line."
                , "Alternatively, this link failure might indicate a bug in GHCi."
                , "If you suspect the latter, please report this as a GHC bug:"
                , "  https://www.haskell.org/ghc/reportabug"
                ])


nameToCLabel :: Name -> String -> FastString
nameToCLabel n suffix = mkFastString label
  where
    encodeZ = zString . zEncodeFS
    (Module pkgKey modName) = assert (isExternalName n) (nameModule n)
    packagePart = encodeZ (unitFS pkgKey)
    modulePart  = encodeZ (moduleNameFS modName)
    occPart     = encodeZ (occNameFS (nameOccName n))

    label = concat
        [ if pkgKey == mainUnit then "" else packagePart ++ "_"
        , modulePart
        , '_':occPart
        , '_':suffix
        ]


-- See Note [Primop wrappers] in GHC.Builtin.PrimOps
primopToCLabel :: PrimOp -> String -> String
primopToCLabel primop suffix = concat
    [ "ghczmprim_GHCziPrim_"
    , zString (zEncodeFS (occNameFS (primOpOcc primop)))
    , '_':suffix
    ]
