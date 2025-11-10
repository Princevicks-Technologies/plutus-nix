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
import           Prelude                (IO, Show, String, putStrLn)
import           Data.Aeson             (ToJSON, FromJSON)
import           GHC.Generics           (Generic)
import qualified Prelude

------------------------------------------------------------
-- ON-CHAIN CODE
------------------------------------------------------------

-- | Loan parameters
data LoanParams = LoanParams
    { lender    :: PubKeyHash
    , borrower  :: PubKeyHash
    , principal :: Integer
    , interest  :: Integer
    , deadline  :: POSIXTime
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.unstableMakeIsData ''LoanParams
PlutusTx.makeLift ''LoanParams

-- | Redeemer: Repay or Claim
data LoanAction = Repay | Claim
    deriving Prelude.Show

PlutusTx.unstableMakeIsData ''LoanAction

{-# INLINABLE mkLoanValidator #-}
mkLoanValidator :: LoanParams -> LoanAction -> ScriptContext -> Bool
mkLoanValidator p action ctx =
    case action of
        -- Borrower repays loan with interest before deadline
        Repay ->
            traceIfFalse "Not signed by borrower" signedByBorrower &&
            traceIfFalse "Repayment too low" repaymentCorrect &&
            traceIfFalse "Too late" beforeDeadline

        -- Lender claims funds after deadline if not repaid
        Claim ->
            traceIfFalse "Not signed by lender" signedByLender &&
            traceIfFalse "Too early to claim" afterDeadline
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBorrower :: Bool
    signedByBorrower = txSignedBy info (borrower p)

    signedByLender :: Bool
    signedByLender = txSignedBy info (lender p)

    beforeDeadline :: Bool
    beforeDeadline = contains (to $ deadline p) $ txInfoValidRange info

    afterDeadline :: Bool
    afterDeadline = contains (from $ deadline p) $ txInfoValidRange info

    repaymentAmount :: Integer
    repaymentAmount = principal p + interest p

    repaymentCorrect :: Bool
    repaymentCorrect =
        let paid = valuePaidTo info (lender p)
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
-- OFF-CHAIN CODE (Playground simulation)
------------------------------------------------------------

data LoanSchema =
        StartLoan LoanParams
    .\/ RepayLoan LoanParams
    .\/ ClaimLoan LoanParams
    deriving Prelude.Show

-- | Borrower locks the loan terms
startLoan :: AsContractError e => LoanParams -> Contract w s e ()
startLoan params = do
    let tx = mustPayToTheScript params $ Ada.lovelaceValueOf (principal params)
    ledgerTx <- submitTxConstraints typedLoanValidator tx
    awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String "Loan initialized"

-- | Borrower repays
repayLoan :: AsContractError e => LoanParams -> Contract w s e ()
repayLoan params = do
    utxos <- utxosAt scrAddress
    let repayment = Ada.lovelaceValueOf (principal params + interest params)
        tx = collectFromScript utxos Repay <>
             mustPayToPubKey (lender params) repayment
    ledgerTx <- submitTxConstraintsSpending typedLoanValidator utxos tx
    awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String "Loan repaid"

-- | Lender claims funds if defaulted
claimLoan :: AsContractError e => LoanParams -> Contract w s e ()
claimLoan params = do
    utxos <- utxosAt scrAddress
    let tx = collectFromScript utxos Claim
    ledgerTx <- submitTxConstraintsSpending typedLoanValidator utxos tx
    awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String "Loan claimed by lender"

endpoints :: Contract () LoanSchema Text ()
endpoints = awaitPromise (start' `select` repay' `select` claim') >> endpoints
  where
    start' = endpoint @"StartLoan" startLoan
    repay' = endpoint @"RepayLoan" repayLoan
    claim' = endpoint @"ClaimLoan" claimLoan

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
    putStrLn "Compiled successfully as LoanContract.hs"
    putStrLn "Validator hash:"
    putStrLn $ "  " ++ Prelude.show valHash
    putStrLn "Script address:"
    putStrLn $ "  " ++ Prelude.show scrAddress
    putStrLn "==============================================="
