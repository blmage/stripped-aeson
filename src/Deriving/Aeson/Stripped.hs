{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Deriving.Aeson.Stripped
       (
         -- $presentation
         --
         -- $setup
         --
         -- $examples

         -- * Core type
         StrippedJSON(..)

         -- * Stripping fields
       , RField
       , CField

         -- * Recovering fields
       , RecoverableValue(..)
       , Coerce
       , FromString
       , FromList
       , Mempty
       , Pure

         -- * Re-exports
       , module Deriving.Aeson
       ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Coerce (Coercible, coerce)
import Data.Functor.Contravariant (Contravariant)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Symbol.Ascii (Head)
import Data.Type.Equality (type (==))
import Deriving.Aeson
import Fcf (type (-), type (=<<), type (>=), Eval, Exp, UnBool, type (||))
import Fcf.Utils (TError)
import Generic.Data.Surgery
import Generic.Data.Surgery.Internal
import GHC.Exts (IsList (..))
import GHC.Generics (Generic, Rep)
import GHC.TypeLits (CmpSymbol, KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal)

import qualified Fcf as Fcf
import qualified GHC.TypeLits as Error


-- | A newtype wrapper which provides 'FromJSON' / 'ToJSON' instances with a specific set
-- of 'Options' (see the "Deriving.Aeson" module), and the ability to strip one or more
-- fields from the JSON output, recovered at decode-time using some default values.
newtype StrippedJSON (fds :: [Type]) (opts :: [Type]) a
    = StrippedJSON { unStrippedJSON :: a }


instance
       ( AesonOptions opts
       , StripFields lt fds l
       , Generic a
       , ToOR f l
       , FromORRepLazy a lt
       , Functor (Arborify l)
       , Contravariant (Arborify l)
       , GFromJSON Zero (Arborify l)
       )
    => FromJSON (StrippedJSON fds opts a) where
    parseJSON
        = fmap (StrippedJSON . fromORLazy @a . recoverFields @lt @fds . toOR')
        . genericParseJSON (aesonOptions @opts)

instance
       ( AesonOptions opts
       , StripFields lt fds l
       , Generic a
       , FromOR f l
       , ToORRepLazy a lt
       , Functor (Arborify l)
       , Contravariant (Arborify l)
       , GToJSON Zero (Arborify l)
       , GToEncoding Zero (Arborify l)
       )
    => ToJSON (StrippedJSON fds opts a) where
    toJSON
        = genericToJSON (aesonOptions @opts)
        . fromOR'
        . stripFields @lt @fds
        . toORLazy @a
        . unStrippedJSON

    toEncoding
        = genericToEncoding (aesonOptions @opts)
        . fromOR'
        . stripFields @lt @fds
        . toORLazy @a
        . unStrippedJSON


-- | A field to be stripped from record values, identified by its @name@.
--
-- The @def@ value is used to recover the field at decode-time (see 'RecoverableValue').
data RField (name :: Symbol) (def :: k)

-- | A field to be stripped from non-record single-constructor values,
-- identified by its __zero-based__ @position@ in the data constructor.
--
-- The @def@ value is used to recover the field at decode-time (see 'RecoverableValue').
data CField (position :: Nat) (def :: k)


-- | A field row from / in which a set of fields can be stripped / recovered.
class StripFields lt fds l | lt fds -> l where
    -- | Strips a set of fields from a field row.
    stripFields :: OR lt x -> OR l x

    -- | Recovers a set of fields in a field row.
    recoverFields :: OR l x -> OR lt x


instance StripFields lt '[] lt where
    stripFields = id
    {-# INLINE stripFields #-}

    recoverFields = id
    {-# INLINE recoverFields #-}

instance
       ( StripFields lt' fds l
       , RmvRField name n t' lt lt'
       , InsRField name n t' lt lt'
       , RecoverableValue def t'
       )
    => StripFields lt (RField name (def :: t) ': fds) l where
    stripFields
        = stripFields @lt' @fds . snd . removeRField @name

    recoverFields
        = insertRField' @name (recoverableValue $ Proxy @def)
        . recoverFields @lt' @fds

instance
       ( fds' ~ DecrementAllAfter idx fds
       , StripFields lt' fds' l
       , RmvCField idx t' lt lt'
       , InsCField idx t' lt lt'
       , RecoverableValue def t'
       )
    => StripFields lt (CField idx (def :: t) ': fds) l where
    stripFields
        = stripFields @lt' @fds'
        . snd
        . removeCField @idx

    recoverFields
        = insertCField' @idx (recoverableValue $ Proxy @def)
        . recoverFields @lt' @fds'


-- | Decrement the index of a 'CField' if it comes after another given field.
type family DecrementAfter (idx :: Nat) (fd :: Type) where
    DecrementAfter idx (CField idx' def)
        = CField (Eval (UnBool (Fcf.Pure idx') (idx' - 1) =<< (idx' >= idx))) def
    DecrementAfter _ fd
        = fd

-- | Decrement the indices of all the 'CField's in a list that come after a given field.
type family DecrementAllAfter (idx :: Nat) (fds :: [Type]) :: [Type] where
    DecrementAllAfter _ '[] = '[]
    DecrementAllAfter idx (fd ': fds) = DecrementAfter idx fd ': DecrementAllAfter idx fds


-- | Recovers a value by 'coerce'-ing it from another 'RecoverableValue'.
data Coerce (x :: k) (a :: Type)

-- | Recovers a 'String'-like value using 'fromString' from 'IsString'.
data FromString (s :: Symbol)

-- | Recovers a list-like value using 'fromList' from 'IsList'.
data FromList (xs :: [k])

-- | Recovers a 'Monoid' value using 'mempty'.
data Mempty

-- | Recovers an 'Applicative' value using 'pure'.
data Pure (x :: k)


-- | A default field value which can be recovered at decode-time.
class RecoverableValue (x :: k) (a :: Type) where
    -- | Recovers a default field value from the type-level.
    recoverableValue :: Proxy x -> a


instance RecoverableValue '() () where
    recoverableValue _ = ()
    {-# INLINE recoverableValue #-}

instance RecoverableValue 'False Bool where
    recoverableValue _ = False
    {-# INLINE recoverableValue #-}

instance RecoverableValue 'True Bool where
    recoverableValue _ = True
    {-# INLINE recoverableValue #-}

instance (KnownNat n, Num a) => RecoverableValue (n :: Nat) a where
    recoverableValue = fromIntegral . natVal
    {-# INLINE recoverableValue #-}

-- | A constraint for requiring a 'Symbol' to represent a valid ASCII 'Char'.
type IsValidChar char =
    ( KnownSymbol char
    , char ~ Head char
    , Eval
        ( UnBool
            (Fcf.Pure (() :: Constraint))
            (TError (Error.ShowType char Error.:<>: Error.Text " is not a valid Char."))
        =<< ((char == "") || (False == (CmpSymbol char (Head char) == 'EQ)))
        )
    )

instance IsValidChar c => RecoverableValue (c :: Symbol) Char where
    recoverableValue = head . recoverableValue
    {-# INLINE recoverableValue #-}

instance KnownSymbol s => RecoverableValue (s :: Symbol) String where
    recoverableValue = symbolVal
    {-# INLINE recoverableValue #-}

instance (KnownSymbol s, IsString a) => RecoverableValue (FromString s) a where
    recoverableValue _ = fromString $ symbolVal $ Proxy @s
    {-# INLINE recoverableValue #-}

instance RecoverableValue '[] [a] where
    recoverableValue _ = []
    {-# INLINE recoverableValue #-}

instance
       ( RecoverableValue x a
       , RecoverableValue xs [a]
       )
    => RecoverableValue (x ': xs) [a] where
    recoverableValue _ = recoverableValue (Proxy @x) : recoverableValue (Proxy @xs)
    {-# INLINE recoverableValue #-}

instance (Coercible a b, RecoverableValue x a) => RecoverableValue (Coerce x a) b where
    recoverableValue _ = coerce @a @b $ recoverableValue $ Proxy @x
    {-# INLINE recoverableValue #-}

instance
       ( IsList a
       , RecoverableValue xs [Item a]
       )
    => RecoverableValue (FromList xs) a where
    recoverableValue _ = fromList $ recoverableValue $ Proxy @xs
    {-# INLINE recoverableValue #-}

instance RecoverableValue 'Nothing (Maybe a) where
    recoverableValue _ = Nothing
    {-# INLINE recoverableValue #-}

instance RecoverableValue x a => RecoverableValue ('Just x) (Maybe a) where
    recoverableValue _ = Just $ recoverableValue $ Proxy @x
    {-# INLINE recoverableValue #-}

instance RecoverableValue x e => RecoverableValue ('Left x) (Either e a) where
    recoverableValue _ = Left $ recoverableValue $ Proxy @x
    {-# INLINE recoverableValue #-}

instance RecoverableValue x a => RecoverableValue ('Right x) (Either e a) where
    recoverableValue _ = Right $ recoverableValue $ Proxy @x
    {-# INLINE recoverableValue #-}

instance
       ( RecoverableValue x a
       , RecoverableValue y b
       )
    => RecoverableValue '(x, y) (a, b) where
    recoverableValue _ =
        ( recoverableValue $ Proxy @x
        , recoverableValue $ Proxy @y
        )
    {-# INLINE recoverableValue #-}

instance
       ( RecoverableValue x a
       , RecoverableValue y b
       , RecoverableValue z c
       )
    => RecoverableValue '(x, y, z) (a, b, c) where
    recoverableValue _ =
        ( recoverableValue $ Proxy @x
        , recoverableValue $ Proxy @y
        , recoverableValue $ Proxy @z
        )
    {-# INLINE recoverableValue #-}

instance
       ( RecoverableValue w a
       , RecoverableValue x b
       , RecoverableValue y c
       , RecoverableValue z d
       )
    => RecoverableValue '(w, x, y, z) (a, b, c, d) where
    recoverableValue _ =
        ( recoverableValue $ Proxy @w
        , recoverableValue $ Proxy @x
        , recoverableValue $ Proxy @y
        , recoverableValue $ Proxy @z
        )
    {-# INLINE recoverableValue #-}

instance
       ( RecoverableValue v a
       , RecoverableValue w b
       , RecoverableValue x c
       , RecoverableValue y d
       , RecoverableValue z e
       )
    => RecoverableValue '(v, w, x, y, z) (a, b, c, d, e) where
    recoverableValue _ =
        ( recoverableValue $ Proxy @v
        , recoverableValue $ Proxy @w
        , recoverableValue $ Proxy @x
        , recoverableValue $ Proxy @y
        , recoverableValue $ Proxy @z
        )
    {-# INLINE recoverableValue #-}

instance
       ( RecoverableValue u a
       , RecoverableValue v b
       , RecoverableValue w c
       , RecoverableValue x d
       , RecoverableValue y e
       , RecoverableValue z f
       )
    => RecoverableValue '(u, v, w, x, y, z) (a, b, c, d, e, f) where
    recoverableValue _ =
        ( recoverableValue $ Proxy @u
        , recoverableValue $ Proxy @v
        , recoverableValue $ Proxy @w
        , recoverableValue $ Proxy @x
        , recoverableValue $ Proxy @y
        , recoverableValue $ Proxy @z
        )
    {-# INLINE recoverableValue #-}

instance (Applicative f, RecoverableValue x a) => RecoverableValue (Pure x) (f a) where
    recoverableValue _ = pure $ recoverableValue $ Proxy @x
    {-# INLINE recoverableValue #-}

instance Monoid m => RecoverableValue Mempty m where
    recoverableValue _ = mempty
    {-# INLINE recoverableValue #-}


-- $presentation
--
-- A layer around the [deriving-aeson](http://hackage.haskell.org/package/deriving-aeson)
-- package, with the ability to strip one or more fields from the JSON output, and recover
-- them when decoding using some specified defaults.

-- $setup
--
-- == Examples
--
-- === Setup
--
-- All the examples below are based on the following setup code:
--
-- >>> :set -XDataKinds
-- >>> :set -XDeriveGeneric
-- >>> :set -XGeneralizedNewtypeDeriving
-- >>> :set -XOverloadedStrings
-- >>> :set -XTypeApplications
--
-- >>> import Data.Aeson
-- >>> import Deriving.Aeson.Stripped
-- >>> import qualified Data.Set as Set
-- >>> import Data.Text (Text)
--
-- >>> :{
--  newtype WrappedInt
--      = WrappedInt Int
--      deriving (FromJSON, Show, ToJSON)
-- :}
--
-- >>> :{
--  data RecordTest = RecordTest
--      { testNumber   :: {-# UNPACK #-} !Int
--      , testNewtype  :: !WrappedInt
--      , testString   :: String
--      , testIsString :: !Text
--      , testList     :: ![Int]
--      , testIsList   :: (Set.Set Int)
--      , testMonoid   :: ![Int]
--      }
--      deriving (Generic, Show)
-- :}
--
-- >>> :{
--  type StrippedRecordFields =
--      '[ RField "testNumber"   7
--       , RField "testNewtype"  (Coerce 42 Int)
--       , RField "testString"   "string"
--       , RField "testIsString" (FromString "text")
--       , RField "testList"     '[ 10, 11, 12 ]
--       , RField "testIsList"   (FromList '[ 13, 14, 13 ])
--       , RField "testMonoid"   Mempty
--       ]
-- :}
--
-- >>> :{
--  data NonRecordTest
--      = NonRecordTest (Either String Int) (Maybe Int) ![Int] (Bool, Char, Int)
--      deriving (Generic, Show)
-- :}
--
-- >>> :{
--  type StrippedNonRecordFields =
--      '[ CField 0 ('Left "test")
--       , CField 2 (Pure 7)
--       , CField 1 'Nothing
--       , CField 3 '( 'False, "z", 42 )
--       ]
-- :}

-- $examples
--
-- === Stripping fields in a record value: ..
--
-- >>> :{
--  encode
--      $ StrippedJSON @StrippedRecordFields @'[]
--      $ RecordTest 1 (WrappedInt 2) "s" "t" [1..3] (Set.fromList [4..6]) [7..9]
-- :}
-- "[]"
--
-- === .. and recovering them when decoding using the specified defaults:
--
-- >>> :{
--  fmap unStrippedJSON
--      $ decode @(StrippedJSON StrippedRecordFields '[] RecordTest)
--      $ encode
--      $ StrippedJSON @StrippedRecordFields @'[]
--      $ RecordTest 1 (WrappedInt 2) "s" "t" [1..3] (Set.fromList [4..6]) [7..9]
-- :}
-- Just (RecordTest {testNumber = 7, testNewtype = WrappedInt 42, testString = "string", testIsString = "text", testList = [10,11,12], testIsList = fromList [13,14], testMonoid = []})
--
-- === Stripping fields in a non-record value: ..
-- >>> :{
--  encode
--      $ StrippedJSON @StrippedNonRecordFields @'[]
--      $ NonRecordTest (Right 1) (Just 2) [3..5] (True, 'a', 6)
-- :}
-- "[]"
--
-- === .. and recovering them when decoding using the specified defaults:
--
-- >>> :{
--  fmap unStrippedJSON
--      $ decode @(StrippedJSON StrippedNonRecordFields '[] NonRecordTest)
--      $ encode
--      $ StrippedJSON @StrippedNonRecordFields @'[]
--      $ NonRecordTest (Right 1) (Just 2) [3..5] (True, 'a', 6)
-- :}
-- Just (NonRecordTest (Left "test") Nothing [7] (False,'z',42))
--
-- === Specifying encoding / decoding options:
--
-- The second parameter to 'StrippedJSON' works exactly the same as the only parameter
-- to 'CustomJSON' (from the
-- [deriving-aeson](http://hackage.haskell.org/package/deriving-aeson) package).
--
-- >>> :{
--  encode
--      $ StrippedJSON @'[] @'[ FieldLabelModifier CamelToSnake ]
--      $ RecordTest 1 (WrappedInt 2) "s" "t" [1..3] (Set.fromList [4..6]) [7..9]
-- :}
-- "{\"test_number\":1,\"test_newtype\":2,\"test_string\":\"s\",\"test_is_string\":\"t\",\"test_list\":[1,2,3],\"test_is_list\":[4,5,6],\"test_monoid\":[7,8,9]}"
