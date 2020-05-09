# stripped-aeson

[![GitHub CI](https://github.com/blmage/stripped-aeson/workflows/CI/badge.svg)](https://github.com/blmage/stripped-aeson/actions)

A layer around the [deriving-aeson](https://hackage.haskell.org/package/deriving-aeson)
package, with the ability to strip one or more fields from the JSON output, and recover
them when decoding using some specified defaults.

### Usage

#### Example

Because an example speaks more than a thousand words, let's start by having
a look at this `Main` module:

```haskell
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}

module Main where

import Data.Aeson
import Data.Set (Set)
import Data.Text (Text)
import Deriving.Aeson.Stripped

newtype WrappedInt
    = WrappedInt Int
    deriving newtype (FromJSON, Num, Show, ToJSON)

data RecordTest = RecordTest
    { testBool     :: !Bool
    , testNumber   :: !Int
    , testNewtype  :: !WrappedInt
    , testString   :: !String
    , testIsString :: !Text
    , testList     :: ![Int]
    , testIsList   :: !(Set Int)
    , testMonoid   :: !Ordering
    , testValue    :: !Double
    }
    deriving (Generic, Show)
    deriving (FromJSON, ToJSON)
        via StrippedJSON
            '[ RField "testBool"     'False
             , RField "testNumber"   7
             , RField "testNewtype"  (Coerce 42 Int)
             , RField "testString"   "string"
             , RField "testIsString" (FromString "text")
             , RField "testList"     '[ 10, 11, 12 ]
             , RField "testIsList"   (FromList '[ 13, 14, 13 ])
             , RField "testMonoid"   Mempty
             ]
            '[ FieldLabelModifier CamelToSnake
             ]
            RecordTest

main :: IO ()
main = do
    let output = encode $ RecordTest True 1 2 "s" "t" [1..3] (fromList [4..6]) GT 3.14

    putStrLn "# Stripping fields:"
    print output
    putStrLn "# And recovering them later:"
    print $ decode @RecordTest output
```
Upon running the corresponding executable, you should get this output :
```
# Stripping fields:
"{\"test_value\":3.14}"
# And recovering them later:
Just (RecordTest {testBool = False, testNumber = 7, testNewtype = 42, testString = "string", testIsString = "text", testList = [10,11,12], testIsList = fromList [13,14], testMonoid = EQ, testValue = 3.14})
```

#### How does it work? The core type: `StrippedJSON` (and the others)

This package revolves entirely around the
[`StrippedJSON`](https://blmage.github.io/stripped-aeson/Deriving-Aeson-Stripped.html#t:StrippedJSON)
newtype, which takes two type parameters:

* a list of instructions indicating which fields should be removed, and how they should
  be recovered:

  | Instruction | Use for | Field identifier |
  | ----------- | ------- | ---------------- |
  | [`RField`](https://blmage.github.io/stripped-aeson/Deriving-Aeson-Stripped.html#t:RField) | record values | name |
  | [`CField`](https://blmage.github.io/stripped-aeson/Deriving-Aeson-Stripped.html#t:CField) | non-record single-constructor values | **zero-based** position |  

  Each instruction takes a **field identifier** and a **default value** used to recover
  the field when decoding.

  See the
  [`RecoverableValue`](https://blmage.github.io/stripped-aeson/Deriving-Aeson-Stripped.html#t:RecoverableValue)
  instances for examples of what can be used as default values. Or don't hesitate to
  roll your own! (PRs welcome!)

* a list of
  [`AesonOptions`](https://hackage.haskell.org/package/deriving-aeson/docs/Deriving-Aeson.html#t:AesonOptions)
  indicating how to customize the JSON output.

  This works exactly the same as the only parameter to 
  [`CustomJSON`](https://hackage.haskell.org/package/deriving-aeson/docs/Deriving-Aeson.html#t:CustomJSON)
  from the [deriving-aeson](https://hackage.haskell.org/package/deriving-aeson) package.

#### Haddock documentation

The Haddock documentation for the package is available
[here](https://blmage.github.io/stripped-aeson/index.html).