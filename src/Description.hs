{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Description (Desc) where

import           Data.Kind    (Type)
import           GHC.Generics (D1, Meta (..), Rep)
import           GHC.TypeLits (AppendSymbol, Symbol)
import           Servant.API  (Description)

type family TypeName (x :: Type) :: Symbol where
  TypeName Int = "integer"
  TypeName String = "string"
  TypeName x = GenericTypeName x (Rep x ())

type family GenericTypeName t (r :: Type) :: Symbol where
  GenericTypeName t (D1 ('MetaData name mod pkg nt) f x) = name

type Desc t n = Description (AppendSymbol (TypeName t) (AppendSymbol " | " n))
