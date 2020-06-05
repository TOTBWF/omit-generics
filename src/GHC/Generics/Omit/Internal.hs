{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-| This is an internal module, which means that the API may change
at any time.
-}

module GHC.Generics.Omit.Internal where

import Data.Kind
import Data.Proxy

import GHC.Generics
import GHC.TypeLits


--------------------------------------------------------------------------------
-- Taken from the "singletons" package
--------------------------------------------------------------------------------

-- | The singleton kind-indexed data family.
data family Sing (a :: k)

-- | A 'SingI' constraint is essentially an implicitly-passed singleton.
class SingI (a :: k) where
  -- | Produce the singleton explicitly. You will likely need the @ScopedTypeVariables@
  -- extension to use this method the way you want.
  sing :: Sing a

-- | The 'SingKind' class is essentially a /kind/ class. It classifies all kinds
-- for which singletons are defined. The class supports converting between a singleton
-- type and the base (unrefined) type which it is built from.
class SingKind k where
  -- | Get a base type from a proxy for the promoted kind. For example,
  -- @DemoteRep Bool@ will be the type @Bool@.
  type DemoteRep k :: Type

  -- | Convert a singleton to its unrefined version.
  fromSing :: Sing (a :: k) -> DemoteRep k

-- | Singleton symbols
data instance Sing (s :: Symbol) where
  SSym :: KnownSymbol s => Sing s

instance KnownSymbol a => SingI a where sing = SSym

instance SingKind Symbol where
  type DemoteRep Symbol = String
  fromSing (SSym :: Sing s) = symbolVal (Proxy :: Proxy s)

-- | Singleton lists
data instance Sing (fields :: [a]) where
    SNil  :: Sing '[]
    SCons :: Sing a -> Sing as -> Sing (a ': as)

instance SingI '[]  where sing = SNil
instance (SingI a, SingI as) => SingI (a ': as)  where sing = SCons sing sing

instance SingKind a => SingKind ([a]) where
  type DemoteRep [a] = [DemoteRep a]
  fromSing SNil = []
  fromSing (SCons a as) = (fromSing a):(fromSing as)

-- | Checks to see if a string is contained in a singleton list.
singElem :: forall proxy fields. (SingI fields) => String -> proxy (fields :: [Symbol]) -> Bool
singElem s proxy = elem s (fromSing (sing :: Sing fields))

-- | Class for generic equality that ignores all fields contained in the symbol list
class GEq (fields :: [Symbol]) (f :: * -> *) where
    geq :: proxy fields -> f a -> f a -> Bool

instance (GEq fields f, SingI fields, Selector c) => GEq fields (S1 c f) where
    geq p a b = singElem (selName a) p || (geq p (unM1 a) (unM1 b))

instance (GEq fields f, GEq fields g) => GEq fields (f :*: g) where
    geq p (f :*: g) (f' :*: g') = geq p f f' && geq p g g'

instance (GEq fields f, GEq fields g) => GEq fields (f :+: g) where
    geq p (L1 f) (L1 f') = geq p f f'
    geq p (R1 g) (R1 g') = geq p g g'
    geq _ _ _            = False

instance (GEq fields f) => GEq fields (D1 c f) where
    geq p (M1 a) (M1 b) = geq p a b

instance (GEq fields f) => GEq fields (C1 c f) where
    geq p (M1 a) (M1 b) = geq p a b

instance (Eq c) => GEq fields (K1 i c) where
    geq p (K1 c) (K1 c') = c == c'

instance GEq fields U1 where
    geq p _ _ = True

newtype Omit (s :: [Symbol]) a = Omit { unOmit :: a }

instance (Generic a, GEq fields (Rep a)) => Eq (Omit fields a) where
    (Omit a) == (Omit b) = geq (Proxy :: Proxy fields) (from a) (from b)
