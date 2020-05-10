{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Main where

import Criterion.Main

import Data.Aeson
import Data.Text (Text)
import Deriving.Aeson.Stripped

import qualified Data.Set as Set


newtype WrappedInt
    = WrappedInt Int
    deriving (FromJSON, ToJSON)


data RecordTest = RecordTest
    { testNumber   :: {-# UNPACK #-}  !Int
    , testNewtype  :: !WrappedInt
    , testString   :: String
    , testIsString :: !Text
    , testList     :: ![Int]
    , testIsList   :: Set.Set Int
    , testMonoid   :: ![Int]
    }
    deriving (Generic)
    deriving (FromJSON, ToJSON) via CustomJSON '[] RecordTest


baseRecord :: RecordTest
baseRecord = RecordTest 1 (WrappedInt 2) "s" "t" [1..3] (Set.fromList [4..6]) [7..9]

strippedRecord0 :: StrippedJSON '[] '[] RecordTest
strippedRecord0 = StrippedJSON baseRecord

type StrippedFields1 = '[ RField "testNumber" 7 ]

strippedRecord1 :: StrippedJSON StrippedFields1 '[] RecordTest
strippedRecord1 = StrippedJSON baseRecord

type StrippedFields2 =
    '[ RField "testNumber"  7
     , RField "testNewtype" (Coerce 42 Int)
     ]

strippedRecord2 :: StrippedJSON StrippedFields2 '[] RecordTest
strippedRecord2 = StrippedJSON baseRecord

type StrippedFields3 =
    '[ RField "testNumber"  7
     , RField "testNewtype" (Coerce 42 Int)
     , RField "testString"  "string"
     ]

strippedRecord3 :: StrippedJSON StrippedFields3 '[] RecordTest
strippedRecord3 = StrippedJSON baseRecord

type StrippedFields4 =
    '[ RField "testNumber"   7
     , RField "testNewtype"  (Coerce 42 Int)
     , RField "testString"   "string"
     , RField "testIsString" (FromString "text")
    ]

strippedRecord4 :: StrippedJSON StrippedFields4 '[] RecordTest
strippedRecord4 = StrippedJSON baseRecord

type StrippedFields5 =
    '[ RField "testNumber"   7
     , RField "testNewtype"  (Coerce 42 Int)
     , RField "testString"   "string"
     , RField "testIsString" (FromString "text")
     , RField "testList"     '[ 10, 11, 12]
     ]

strippedRecord5 :: StrippedJSON StrippedFields5 '[] RecordTest
strippedRecord5 = StrippedJSON baseRecord

type StrippedFields6 =
    '[ RField "testNumber"   7
     , RField "testNewtype"  (Coerce 42 Int)
     , RField "testString"   "string"
     , RField "testIsString" (FromString "text")
     , RField "testList"     '[ 10, 11, 12]
     , RField "testIsList"   (FromList '[ 13, 14, 13])
     ]

strippedRecord6 :: StrippedJSON StrippedFields6 '[] RecordTest
strippedRecord6 = StrippedJSON baseRecord

type StrippedFields7 =
    '[ RField "testNumber"   7
     , RField "testNewtype"  (Coerce 42 Int)
     , RField "testString"   "string"
     , RField "testIsString" (FromString "text")
     , RField "testList"     '[ 10, 11, 12]
     , RField "testIsList"   (FromList '[ 13, 14, 13])
     , RField "testMonoid"   Mempty
     ]

strippedRecord7 :: StrippedJSON StrippedFields7 '[] RecordTest
strippedRecord7 = StrippedJSON baseRecord


roundTrip
    :: forall fds opts
     . ( FromJSON (StrippedJSON fds opts RecordTest)
       , ToJSON   (StrippedJSON fds opts RecordTest)
       )
    => StrippedJSON fds opts RecordTest
    -> Maybe (StrippedJSON fds opts RecordTest)
roundTrip = decode @(StrippedJSON fds opts RecordTest) . encode


main = defaultMain
    [ bgroup "encode"
        [ bench "base"      $ whnf encode baseRecord
        , bench "stripped0" $ whnf encode strippedRecord0
        , bench "stripped1" $ whnf encode strippedRecord1
        , bench "stripped2" $ whnf encode strippedRecord2
        , bench "stripped3" $ whnf encode strippedRecord3
        , bench "stripped4" $ whnf encode strippedRecord4
        , bench "stripped5" $ whnf encode strippedRecord5
        , bench "stripped6" $ whnf encode strippedRecord6
        , bench "stripped7" $ whnf encode strippedRecord7
        ]
    , bgroup "round-trip"
        [ bench "base"      $ whnf (decode @RecordTest . encode) baseRecord
        , bench "stripped0" $ whnf roundTrip strippedRecord0
        , bench "stripped1" $ whnf roundTrip strippedRecord1
        , bench "stripped2" $ whnf roundTrip strippedRecord2
        , bench "stripped3" $ whnf roundTrip strippedRecord3
        , bench "stripped4" $ whnf roundTrip strippedRecord4
        , bench "stripped5" $ whnf roundTrip strippedRecord5
        , bench "stripped6" $ whnf roundTrip strippedRecord6
        , bench "stripped7" $ whnf roundTrip strippedRecord7
        ]
    ]
