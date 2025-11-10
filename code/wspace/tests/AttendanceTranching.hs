
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Plutus.V2.Ledger.Api
    ( BuiltinData
    , ScriptContext (..)
    , Validator
    , mkValidatorScript
    , PubKeyHash (..)
    , TxInfo (..)
    , scriptContextTxInfo
    , txInfoSignatories
    , ValidatorHash
    , Address (..)
    , Credential (..)
    , StakingCredential
    , BuiltinByteString
    , unValidatorScript
    )
import PlutusTx
import PlutusTx.Prelude hiding ((<>))   -- 👈 hide PlutusTx’s version of <>
import Prelude (IO, putStrLn, Show(..), String, FilePath, (<>))
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as LBS
import qualified Codec.Serialise as Serialise

------------------------------------------------------------
-- | DATUM
------------------------------------------------------------
data AttendanceDatum = AttendanceDatum
    { userAddress  :: PubKeyHash
    , totalDays    :: Integer
    , trancheLevel :: Integer
    , rewardAmount :: Integer
    }
    deriving stock (Show, Generic)

PlutusTx.makeIsDataIndexed ''AttendanceDatum [('AttendanceDatum, 0)]

------------------------------------------------------------
-- | REDEEMER
------------------------------------------------------------
data AttendanceAction
    = MarkAttendance
    | ClaimTranche
    deriving stock (Show, Generic)

PlutusTx.makeIsDataIndexed ''AttendanceAction
    [ ('MarkAttendance, 0)
    , ('ClaimTranche, 1)
    ]

------------------------------------------------------------
-- | VALIDATOR LOGIC
------------------------------------------------------------
{-# INLINABLE mkAttendanceValidator #-}
mkAttendanceValidator :: AttendanceDatum -> AttendanceAction -> ScriptContext -> Bool
mkAttendanceValidator datum redeemer ctx =
    case redeemer of
        MarkAttendance -> traceIfFalse "Not authorized to mark attendance" authorized
        ClaimTranche   -> traceIfFalse "Not eligible to claim" canClaim
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signers :: [PubKeyHash]
    signers = txInfoSignatories info

    oraclePkh :: PubKeyHash
    oraclePkh = PubKeyHash ("abcd1234abcd1234abcd1234abcd1234abcd1234abcd1234abcd1234" :: BuiltinByteString)

    authorized :: Bool
    authorized = oraclePkh `elem` signers

    canClaim :: Bool
    canClaim =
        totalDays datum >= 5 &&
        userAddress datum `elem` signers

------------------------------------------------------------
-- | WRAPPER + VALIDATOR
------------------------------------------------------------
{-# INLINABLE mkWrapped #-}
mkWrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrapped d r c =
    check $
        mkAttendanceValidator
            (unsafeFromBuiltinData d)
            (unsafeFromBuiltinData r)
            (unsafeFromBuiltinData c)

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| mkWrapped ||])

------------------------------------------------------------
-- | ADDRESS (Manual Construction)
------------------------------------------------------------
{-# INLINABLE validatorAddress #-}
validatorAddress :: ValidatorHash -> Maybe StakingCredential -> Address
validatorAddress vh mStake = Address (ScriptCredential vh) mStake

------------------------------------------------------------
-- | Example usage
------------------------------------------------------------
exampleValidatorHash :: ValidatorHash
exampleValidatorHash =
    "00000000000000000000000000000000000000000000000000000000"

exampleAddress :: Address
exampleAddress = validatorAddress exampleValidatorHash Nothing

------------------------------------------------------------
-- | SERIALIZATION: Save Validator to .plutus File
------------------------------------------------------------
saveValidator :: FilePath -> Validator -> IO ()
saveValidator file val = do
    let script = unValidatorScript val
        serialised = Serialise.serialise script
    LBS.writeFile file serialised
    putStrLn $ " Validator serialized to: " <> file

------------------------------------------------------------
-- | MAIN (Executable Entry Point)
------------------------------------------------------------
main :: IO ()
main = do
    putStrLn "==========================================="
    putStrLn "      Attendance Tranching Validator     "
    putStrLn "==========================================="
    putStrLn ""
    putStrLn $ "Validator Hash (Example): " <> show exampleValidatorHash
    putStrLn $ "Validator Address: " <> show exampleAddress
    putStrLn ""
    saveValidator "attendanceTranching.plutus" validator
    putStrLn " Script compiled successfully and saved as attendanceTranching.plutus"
