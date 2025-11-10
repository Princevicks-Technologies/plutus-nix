{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger
import           Ledger.Typed.Scripts   as Scripts
import           Ledger.Ada             as Ada
import           Playground.Contract
import           Plutus.Contract
import           Prelude                (IO, Show, String, putStrLn, (++))
import qualified Prelude
import           Data.Aeson             (ToJSON, FromJSON)
import           GHC.Generics           (Generic)

------------------------------------------------------------
-- ON-CHAIN CODE
------------------------------------------------------------

-- Loan parameters
data LoanParams = LoanParams
    { lender    :: PubKeyHash
    , borrower  :: PubKeyHash
    , principal :: Integer
    , interest  :: Integer
    , deadline  :: POSIXTime
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.unstableMakeIsData ''LoanParams
PlutusTx.makeLift ''LoanParams

-- Redeemer
data LoanAction = Repay | Claim
    deriving Prelude.Show

PlutusTx.unstableMakeIsData ''LoanAction

-- Validator logic
{-# INLINABLE mkLoanValidator #-}
mkLoanValidator :: LoanParams -> LoanAction -> ScriptContext -> Bool
mkLoanValidator p action ctx =
    case action of
        Repay -> traceIfFalse "Not signed by borrower" signedByBorrower &&
                 traceIfFalse "Repayment too low" repaymentCorrect &&
                 traceIfFalse "Too late" beforeDeadline
        Claim -> traceIfFalse "Not signed by lender" signedByLender &&
                 traceIfFalse "Too early to claim" afterDeadline
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBorrower = txSignedBy info (borrower p)
    signedByLender   = txSignedBy info (lender p)
    beforeDeadline   = contains (to $ deadline p) $ txInfoValidRange info
    afterDeadline    = contains (from $ deadline p) $ txInfoValidRange info
    repaymentAmount  = principal p + interest p
    repaymentCorrect = let paid = valuePaidTo info (lender p)
                       in Ada.getLovelace (Ada.fromValue paid) >= repaymentAmount

------------------------------------------------------------
-- TYPED SCRIPT WRAPPER
------------------------------------------------------------

data Loan
instance Scripts.ValidatorTypes Loan where
    type DatumType Loan = LoanParams
    type RedeemerType Loan = LoanAction

typedLoanValidator :: Scripts.TypedValidator Loan
typedLoanValidator = Scripts.mkTypedValidator @Loan
    $$(PlutusTx.compile [|| mkLoanValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @LoanParams @LoanAction

validator :: Validator
validator = Scripts.validatorScript typedLoanValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedLoanValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

------------------------------------------------------------
-- OFF-CHAIN CODE
------------------------------------------------------------

-- Modern Plutus endpoints
type LoanSchema =
        Endpoint "StartLoan" LoanParams
    -- Repay and Claim endpoints handled via selectList

-- Borrower starts loan
startLoan :: AsContractError e => LoanParams -> Contract w s e ()
startLoan params = do
    let tx = mustPayToTheScript params $ Ada.lovelaceValueOf (principal params)
    ledgerTx <- submitTxConstraints typedLoanValidator tx
    awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String "✅ Loan initialized."

-- Borrower repays loan
repayLoan :: AsContractError e => LoanParams -> Contract w s e ()
repayLoan params = do
    utxos <- utxosAt scrAddress
    let repayment = Ada.lovelaceValueOf (principal params + interest params)
        tx = collectFromScript utxos Repay <>
             mustPayToPubKey (lender params) repayment
    ledgerTx <- submitTxConstraintsSpending typedLoanValidator utxos tx
    awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String "💰 Loan repaid."

-- Lender claims funds if defaulted
claimLoan :: AsContractError e => LoanParams -> Contract w s e ()
claimLoan params = do
    utxos <- utxosAt scrAddress
    let tx = collectFromScript utxos Claim
    ledgerTx <- submitTxConstraintsSpending typedLoanValidator utxos tx
    awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String "⚠️ Loan claimed by lender."

-- Endpoint dispatcher
endpoints :: Contract () LoanSchema Text ()
endpoints = do
    logInfo @String "🏦 Loan contract ready..."
    selectList [ endpoint @"StartLoan" startLoan
               , endpoint @"RepayLoan" repayLoan
               , endpoint @"ClaimLoan" claimLoan
               ] >> endpoints

mkSchemaDefinitions ''LoanSchema
mkKnownCurrencies []

------------------------------------------------------------
-- MAIN ENTRY POINT
------------------------------------------------------------

main :: IO ()
main = do
    putStrLn "==============================================="
    putStrLn "     💸 Loan / Borrow Smart Contract (Plutus)  "
    putStrLn "==============================================="
    putStrLn "This contract allows a lender to issue loans"
    putStrLn "and a borrower to repay with interest or default."
    putStrLn $ "Validator Hash: " ++ Prelude.show valHash
    putStrLn $ "Script Address: " ++ Prelude.show scrAddress
    putStrLn "==============================================="
