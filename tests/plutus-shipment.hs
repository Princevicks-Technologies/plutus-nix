{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P

-- Plutus core
import Plutus.V2.Ledger.Api
    ( Validator
    , ScriptContext
    , TxInfo
    , PubKeyHash
    , POSIXTime
    , scriptContextTxInfo
    , mkValidatorScript
    , BuiltinData
    )
import Plutus.V2.Ledger.Contexts (txSignedBy, valuePaidTo, txInfoValidRange)
import Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins

-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS

--------------------------------------------------------------------------------
-- On-chain datatypes
--------------------------------------------------------------------------------

data Milestone = Milestone
    { mIdx   :: Integer
    , mProof :: BuiltinByteString
    }
PlutusTx.unstableMakeIsData ''Milestone

data ShipDatum = ShipDatum
    { sdRoute           :: [BuiltinByteString]
    , sdCustodian       :: PubKeyHash
    , sdCarrier         :: PubKeyHash
    , sdStatus          :: Integer
    , sdDeadline        :: POSIXTime
    , sdTotalMilestones :: Integer
    , sdEscrow          :: Integer
    }
PlutusTx.unstableMakeIsData ''ShipDatum

data ShipRedeemer
    = InitFund
    | SubmitMilestone Milestone
    | ReclaimByShipper
PlutusTx.unstableMakeIsData ''ShipRedeemer

--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

{-# INLINABLE divideInteger #-}
divideInteger :: Integer -> Integer -> Integer
divideInteger n d = n `divide` d

{-# INLINABLE valuePaidToPkh #-}
valuePaidToPkh :: TxInfo -> PubKeyHash -> Integer
valuePaidToPkh info pkh =
    valueOf (valuePaidTo info pkh) adaSymbol adaToken

--------------------------------------------------------------------------------
-- Validator logic
--------------------------------------------------------------------------------

{-# INLINABLE mkShipmentValidator #-}
mkShipmentValidator :: ShipDatum -> ShipRedeemer -> ScriptContext -> Bool
mkShipmentValidator datum redeemer ctx =
    case redeemer of
        InitFund           -> validateInit
        SubmitMilestone ms -> validateSubmit ms
        ReclaimByShipper   -> validateReclaim
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    txSignedBy' :: PubKeyHash -> Bool
    txSignedBy' = txSignedBy info

    validateInit :: Bool
    validateInit =
        txSignedBy' (sdCustodian datum)

    validateSubmit :: Milestone -> Bool
    validateSubmit ms =
        let oldIdx = sdStatus datum
            newIdx = mIdx ms
            total = sdTotalMilestones datum
            escrow = sdEscrow datum
            perMilestone = if total > 0 then divideInteger escrow total else 0
            paidToCarrier = valuePaidToPkh info (sdCarrier datum)
            beforeDeadline = Interval.contains (to $ sdDeadline datum) (txInfoValidRange info)
        in  txSignedBy' (sdCarrier datum)
            && newIdx == oldIdx + 1
            && beforeDeadline
            && paidToCarrier >= perMilestone

    validateReclaim :: Bool
    validateReclaim =
        let total = sdTotalMilestones datum
            completed = sdStatus datum
            afterDeadline = Interval.contains (from $ sdDeadline datum + 1) (txInfoValidRange info)
        in  txSignedBy' (sdCustodian datum)
            && completed < total
            && afterDeadline

--------------------------------------------------------------------------------
-- Boilerplate
--------------------------------------------------------------------------------

{-# INLINABLE wrappedValidator #-}
wrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedValidator d r ctx =
    let datum    = PlutusTx.unsafeFromBuiltinData @ShipDatum d
        redeemer = PlutusTx.unsafeFromBuiltinData @ShipRedeemer r
        context  = PlutusTx.unsafeFromBuiltinData @ScriptContext ctx
    in if mkShipmentValidator datum redeemer context
       then ()
       else PlutusTx.Prelude.traceError "Shipment validation failed"

validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [|| wrappedValidator ||])

--------------------------------------------------------------------------------
-- Serialization helper
--------------------------------------------------------------------------------

writePlutusScript :: FilePath -> Validator -> IO ()
writePlutusScript path val = do
    let bytes = LBS.toStrict $ Serialise.serialise val
    BS.writeFile path bytes
    putStrLn $ "Wrote plutus script to: " <> path

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
    let out = "shipment.plutus"
    writePlutusScript out validator
    putStrLn "Shipment Plutus V2 validator written."
