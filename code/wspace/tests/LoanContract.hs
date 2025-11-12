{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

-- Plutus / on-chain
import qualified PlutusTx
import           PlutusTx.Prelude        hiding (Semigroup(..), unless)
import qualified PlutusTx
import qualified PlutusTx.Builtins      as Builtins

import qualified Plutus.V2.Ledger.Api   as V2
import qualified Plutus.V2.Ledger.Contexts as Contexts

-- serialization / cardano-api
import qualified Data.ByteString.Short  as SBS
import qualified Data.ByteString.Lazy   as LBS
import qualified Codec.Serialise        as Serialise
import           Cardano.Api.Shelley    (PlutusScript (..), PlutusScriptV2)
import           Cardano.Api            (writeFileTextEnvelope, displayError)

-- utils
import           GHC.Generics           (Generic)
import qualified Prelude                as P
import           Prelude                (IO)

--------------------------------------------------------------------------------
-- Data types (datum / redeemer)
--------------------------------------------------------------------------------

-- Loan parameters stored as the datum
data LoanParams = LoanParams
    { lpLender    :: V2.PubKeyHash
    , lpBorrower  :: V2.PubKeyHash
    , lpPrincipal :: Integer
    , lpInterest  :: Integer
    , lpDeadline  :: V2.POSIXTime
    }
    deriving (P.Show, Generic)

PlutusTx.unstableMakeIsData ''LoanParams
PlutusTx.makeLift ''LoanParams

-- Redeemer: borrower repays, lender claims after deadline
data LoanAction = Repay | Claim
    deriving (P.Show, Generic)

PlutusTx.unstableMakeIsData ''LoanAction

--------------------------------------------------------------------------------
-- Validator logic (on-chain)
--
-- Note: simple checks for signer & time window. Add value checks if you want
--------------------------------------------------------------------------------
{-# INLINABLE mkLoanValidator #-}
mkLoanValidator :: LoanParams -> LoanAction -> V2.ScriptContext -> Bool
mkLoanValidator datum action ctx =
    case action of
        Repay ->
            traceIfFalse "Repay: not signed by borrower" signedByBorrower &&
            traceIfFalse "Repay: too late" beforeDeadline

        Claim ->
            traceIfFalse "Claim: not signed by lender" signedByLender &&
            traceIfFalse "Claim: too early" afterDeadline
  where
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo ctx

    signedByBorrower :: Bool
    signedByBorrower = V2.txSignedBy info (lpBorrower datum)

    signedByLender :: Bool
    signedByLender = V2.txSignedBy info (lpLender datum)

    beforeDeadline :: Bool
    beforeDeadline = contains (to $ lpDeadline datum) $ V2.txInfoValidRange info

    afterDeadline :: Bool
    afterDeadline = contains (from $ lpDeadline datum) $ V2.txInfoValidRange info

--------------------------------------------------------------------------------
-- Wrap validator for compilation
--------------------------------------------------------------------------------
{-# INLINABLE mkWrapped #-}
mkWrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrapped d r c =
    check $
      mkLoanValidator
        (PlutusTx.unsafeFromBuiltinData d)
        (PlutusTx.unsafeFromBuiltinData r)
        (PlutusTx.unsafeFromBuiltinData c)

-- compiled code (for Plutus V2)
loanValidatorCode :: PlutusTx.CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
loanValidatorCode = $$(PlutusTx.compile [|| mkWrapped ||])

-- Script objects
loanScript :: V2.Script
loanScript = V2.fromCompiledCode loanValidatorCode

loanValidator :: V2.Validator
loanValidator = V2.mkValidatorScript loanValidatorCode

--------------------------------------------------------------------------------
-- Serialization to Cardano Api PlutusScript for saving
--------------------------------------------------------------------------------
loanScriptSBS :: SBS.ShortByteString
loanScriptSBS = SBS.toShort . LBS.toStrict $ Serialise.serialise loanScript

loanPlutusScript :: PlutusScript PlutusScriptV2
loanPlutusScript = PlutusScriptSerialised loanScriptSBS

--------------------------------------------------------------------------------
-- Utilities: validator hash & address (simple derivation)
--------------------------------------------------------------------------------
loanValidatorHash :: V2.ValidatorHash
loanValidatorHash = V2.ValidatorHash loanScriptSBS

loanAddress :: V2.Address
loanAddress = V2.Address (V2.ScriptCredential loanValidatorHash) Nothing

--------------------------------------------------------------------------------
-- Write .plutus file and main
--------------------------------------------------------------------------------
saveToFile :: FilePath -> IO ()
saveToFile path = do
    let desc = "Loan contract (compiled PlutusV2 script)"
    r <- writeFileTextEnvelope path (Just desc) loanPlutusScript
    case r of
      Left err -> P.putStrLn $ "Error writing script: " P.++ P.show (displayError err)
      Right () -> P.putStrLn $ "Wrote script to " P.++ path

main :: IO ()
main = do
    P.putStrLn "=========================================="
    P.putStrLn " LoanContract (Plutus V2) compiled"
    P.putStrLn "=========================================="
    P.putStrLn $ "ValidatorHash (short bytes): " P.++ P.show loanValidatorHash
    P.putStrLn $ "Address: " P.++ P.show loanAddress
    saveToFile "loanContract.plutus"
    P.putStrLn "Done."
