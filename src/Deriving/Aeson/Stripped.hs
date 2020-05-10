{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
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

import qualified Fcf
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
        = insertRField' @name (recoverValue $ Proxy @def)
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
        = insertCField' @idx (recoverValue $ Proxy @def)
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
--
-- >>> recoverValue (Proxy @(Coerce 7 Int)) :: WrappedInt
-- WrappedInt 7
data Coerce (x :: k) (a :: Type)

-- | Recovers a 'String'-like value using 'fromString' from 'IsString'.
--
-- >>> recoverValue (Proxy @(FromString "text")) :: Text
-- "text"
data FromString (s :: Symbol)

-- | Recovers a list-like value using 'fromList' from 'IsList'.
--
-- >>> recoverValue (Proxy @(FromList '[1,2,3])) :: [Int]
-- [1,2,3]
data FromList (xs :: [k])

-- | Recovers a 'Monoid' value using 'mempty'.
--
-- >>> recoverValue (Proxy @(Pure 1)) :: Maybe Int
-- Just 1
data Mempty

-- | Recovers an 'Applicative' value using 'pure'.
--
-- >>> recoverValue (Proxy @(Pure 1)) :: Maybe Int
-- Just 1
data Pure (x :: k)


-- | A default field value which can be recovered at decode-time.
class RecoverableValue (x :: k) (a :: Type) where
    -- | Recovers a default field value from the type-level.
    recoverValue :: Proxy x -> a


instance RecoverableValue '() () where
    recoverValue _ = ()
    {-# INLINE recoverValue #-}

instance RecoverableValue 'False Bool where
    recoverValue _ = False
    {-# INLINE recoverValue #-}

instance RecoverableValue 'True Bool where
    recoverValue _ = True
    {-# INLINE recoverValue #-}

instance (KnownNat n, Num a) => RecoverableValue (n :: Nat) a where
    recoverValue = fromIntegral . natVal
    {-# INLINE recoverValue #-}

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
    recoverValue = head . recoverValue
    {-# INLINE recoverValue #-}

instance KnownSymbol s => RecoverableValue (s :: Symbol) String where
    recoverValue = symbolVal
    {-# INLINE recoverValue #-}

instance (KnownSymbol s, IsString a) => RecoverableValue (FromString s) a where
    recoverValue _ = fromString $ symbolVal $ Proxy @s
    {-# INLINE recoverValue #-}

instance RecoverableValue '[] [a] where
    recoverValue _ = []
    {-# INLINE recoverValue #-}

instance
       ( RecoverableValue x a
       , RecoverableValue xs [a]
       )
    => RecoverableValue (x ': xs) [a] where
    recoverValue _ = recoverValue (Proxy @x) : recoverValue (Proxy @xs)
    {-# INLINE recoverValue #-}

instance (Coercible a b, RecoverableValue x a) => RecoverableValue (Coerce x a) b where
    recoverValue _ = coerce @a @b $ recoverValue $ Proxy @x
    {-# INLINE recoverValue #-}

instance
       ( IsList a
       , RecoverableValue xs [Item a]
       )
    => RecoverableValue (FromList xs) a where
    recoverValue _ = fromList $ recoverValue $ Proxy @xs
    {-# INLINE recoverValue #-}

instance RecoverableValue 'Nothing (Maybe a) where
    recoverValue _ = Nothing
    {-# INLINE recoverValue #-}

instance RecoverableValue x a => RecoverableValue ('Just x) (Maybe a) where
    recoverValue _ = Just $ recoverValue $ Proxy @x
    {-# INLINE recoverValue #-}

instance RecoverableValue x e => RecoverableValue ('Left x) (Either e a) where
    recoverValue _ = Left $ recoverValue $ Proxy @x
    {-# INLINE recoverValue #-}

instance RecoverableValue x a => RecoverableValue ('Right x) (Either e a) where
    recoverValue _ = Right $ recoverValue $ Proxy @x
    {-# INLINE recoverValue #-}

instance
       ( RecoverableValue x a
       , RecoverableValue y b
       )
    => RecoverableValue '(x, y) (a, b) where
    recoverValue _ =
        ( recoverValue $ Proxy @x
        , recoverValue $ Proxy @y
        )
    {-# INLINE recoverValue #-}

instance
       ( RecoverableValue x a
       , RecoverableValue y b
       , RecoverableValue z c
       )
    => RecoverableValue '(x, y, z) (a, b, c) where
    recoverValue _ =
        ( recoverValue $ Proxy @x
        , recoverValue $ Proxy @y
        , recoverValue $ Proxy @z
        )
    {-# INLINE recoverValue #-}

instance
       ( RecoverableValue w a
       , RecoverableValue x b
       , RecoverableValue y c
       , RecoverableValue z d
       )
    => RecoverableValue '(w, x, y, z) (a, b, c, d) where
    recoverValue _ =
        ( recoverValue $ Proxy @w
        , recoverValue $ Proxy @x
        , recoverValue $ Proxy @y
        , recoverValue $ Proxy @z
        )
    {-# INLINE recoverValue #-}

instance
       ( RecoverableValue v a
       , RecoverableValue w b
       , RecoverableValue x c
       , RecoverableValue y d
       , RecoverableValue z e
       )
    => RecoverableValue '(v, w, x, y, z) (a, b, c, d, e) where
    recoverValue _ =
        ( recoverValue $ Proxy @v
        , recoverValue $ Proxy @w
        , recoverValue $ Proxy @x
        , recoverValue $ Proxy @y
        , recoverValue $ Proxy @z
        )
    {-# INLINE recoverValue #-}

instance
       ( RecoverableValue u a
       , RecoverableValue v b
       , RecoverableValue w c
       , RecoverableValue x d
       , RecoverableValue y e
       , RecoverableValue z f
       )
    => RecoverableValue '(u, v, w, x, y, z) (a, b, c, d, e, f) where
    recoverValue _ =
        ( recoverValue $ Proxy @u
        , recoverValue $ Proxy @v
        , recoverValue $ Proxy @w
        , recoverValue $ Proxy @x
        , recoverValue $ Proxy @y
        , recoverValue $ Proxy @z
        )
    {-# INLINE recoverValue #-}

instance (Applicative f, RecoverableValue x a) => RecoverableValue (Pure x) (f a) where
    recoverValue _ = pure $ recoverValue $ Proxy @x
    {-# INLINE recoverValue #-}

instance Monoid m => RecoverableValue Mempty m where
    recoverValue _ = mempty
    {-# INLINE recoverValue #-}


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
-- All the examples on this page are based on the following setup code:
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
