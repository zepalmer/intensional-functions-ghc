{-# LANGUAGE GADTs, TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes     #-} -- for unXRec, etc.
{-# LANGUAGE CPP                     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveDataTypeable      #-}
{-# LANGUAGE EmptyCase               #-}
{-# LANGUAGE EmptyDataDeriving       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE UndecidableInstances    #-}

module T18324b where


data L a e = L a e
unLoc :: L a e -> e
unLoc (L _ e)  = e

data B = B


type family XRec p a = r | r -> a
type instance XRec (GhcPass p) a = L (Anno a) a

type family Anno a = b 

data GhcPass (pass :: Pass)
data Pass = Rn

type family IdGhcP (pass :: Pass) where
  IdGhcP 'Rn     = B


type GhcRn = GhcPass 'Rn

data LHsType pass

data ClsInstDecl pass =
  ClsInstDecl
  { -- cid_tyfam_insts   :: [LTyFamInstDecl pass]
  -- , 
    cid_datafam_insts :: [LDataFamInstDecl pass]
  }


-- type LTyFamInstDecl pass = XRec pass (TyFamInstDecl pass)
type LDataFamInstDecl pass = XRec pass ([FamEqn pass (HsDataDefn pass)])
-- type TyFamDefltDecl = TyFamInstDecl

type family IdP p
type instance IdP (GhcPass p) = IdGhcP p

type LIdP p = XRec p (IdP p)

data HsDataDefn pass 

data FamEqn pass rhs
  = FamEqn
       { feqn_tycon  :: LIdP pass

-- LIdP (GhcRn) ~~> 

       , feqn_rhs    :: rhs }

-- type TyFamInstEqn pass = FamEqn pass (LHsType pass)

-- data TyFamInstDecl pass
--   = TyFamInstDecl { tfid_eqn :: TyFamInstEqn pass }


fffggg :: ClsInstDecl GhcRn -> [Int]
fffggg ddd = -- let
    -- data_fams = 
      do
        [FamEqn { feqn_tycon = L l _
                , feqn_rhs   = defn }] <- unLoc <$> cid_datafam_insts ddd
        [ 0 ]
    -- in
    --   data_fams
    -- ty_fams = do
    --   TyFamInstDecl { tfid_eqn = FamEqn { feqn_tycon = L l _ } } <- unLoc <$> cid_tyfam_insts ddd
    --   [ 0 ]
    -- in data_fams ++ ty_fams
