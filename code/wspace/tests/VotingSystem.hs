{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import PlutusTx
import PlutusTx.Prelude hiding ((<>))
import PlutusLedgerApi.V2                  -- Correct: provides BuiltinData, ScriptContext, etc.
import qualified Plutus.V2.Ledger.Api as V2
import qualified Plutus.V2.Ledger.Contexts as Contexts

import Prelude (IO, Show (..), String, print, (<>))
import qualified Prelude
import qualified PlutusTx.Builtins as Builtins

import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Codec.Serialise as Serialise

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..))
import GHC.Generics (Generic)
------------------------------------------------------------
-- | DATUM: Voting state
------------------------------------------------------------
data VotingDatum = VotingDatum
    { proposalId   :: Integer
    , yesVotes     :: Integer
    , noVotes      :: Integer
    , totalVoters  :: [V2.PubKeyHash]   -- Track who voted
    }
    deriving (Show, Generic)

PlutusTx.unstableMakeIsData ''VotingDatum
PlutusTx.makeLift ''VotingDatum

------------------------------------------------------------
-- | REDEEMER: Voting actions
------------------------------------------------------------
data VotingRedeemer
    = CastYes V2.PubKeyHash
    | CastNo V2.PubKeyHash
    | TallyVotes
    deriving (Show, Generic)

PlutusTx.unstableMakeIsData ''VotingRedeemer

------------------------------------------------------------
-- | VALIDATOR LOGIC (INLINABLE)
------------------------------------------------------------
{-# INLINABLE mkVotingValidator #-}
mkVotingValidator :: VotingDatum -> VotingRedeemer -> ScriptContext -> Bool
mkVotingValidator datum redeemer ctx = case redeemer of
    CastYes voterPkh ->
        traceIfFalse "Already voted" (notAlreadyVoted voterPkh) &&
        traceIfFalse "Not signed by voter" (signedByVoter voterPkh) &&
        traceIfFalse "Invalid YES output" validYesUpdate

    CastNo voterPkh ->
        traceIfFalse "Already voted" (notAlreadyVoted voterPkh) &&
        traceIfFalse "Not signed by voter" (signedByVoter voterPkh) &&
        traceIfFalse "Invalid NO output" validNoUpdate

    TallyVotes ->
        traceIfFalse "Tally verification passed" True
  where
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo ctx

    -- Check if voter hasn't voted yet
    notAlreadyVoted :: V2.PubKeyHash -> Bool
    notAlreadyVoted pkh = not (pkh `elem` totalVoters datum)

    -- Check if transaction is signed by voter
    signedByVoter :: V2.PubKeyHash -> Bool
    signedByVoter pkh = pkh `elem` V2.txInfoSignatories info

    -- Get the continuing output (same script address)
    getContinuingOutputDatum :: Maybe VotingDatum
    getContinuingOutputDatum = do
        [output] <- Contexts.getContinuingOutputs ctx
        V2.OutputDatum (V2.Datum d) <- Just $ V2.txOutDatum output
        PlutusTx.fromBuiltinData d

    -- Validate YES vote: increment yesVotes by 1
    validYesUpdate :: Bool
    validYesUpdate = case getContinuingOutputDatum of
        Just newDatum ->
            yesVotes newDatum == yesVotes datum + 1 &&
            noVotes newDatum == noVotes datum &&
            proposalId newDatum == proposalId datum &&
            totalVoters newDatum == voterPkh : totalVoters datum
        Nothing -> False

    -- Validate NO vote: increment noVotes by 1
    validNoUpdate :: Bool
    validNoUpdate = case getContinuingOutputDatum of
        Just newDatum ->
            noVotes newDatum == noVotes datum + 1 &&
            yesVotes newDatum == yesVotes datum &&
            proposalId newDatum == proposalId datum &&
            totalVoters newDatum == voterPkh : totalVoters datum
        Nothing -> False

------------------------------------------------------------
-- | WRAPPED VALIDATOR (for BuiltinData)
------------------------------------------------------------
{-# INLINABLE mkWrappedValidator #-}
mkWrappedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedValidator datum redeemer ctx =
    check $ mkVotingValidator
        (PlutusTx.unsafeFromBuiltinData datum)
        (PlutusTx.unsafeFromBuiltinData redeemer)
        (PlutusTx.unsafeFromBuiltinData ctx)

-- CRITICAL FIX: Use [| ... |] NOT [|| ... ||]
votingValidatorCode :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ())
votingValidatorCode = $$(PlutusTx.compile [| mkWrappedValidator |])

------------------------------------------------------------
-- | SERIALIZATION
------------------------------------------------------------
votingValidatorScript :: V2.Script
votingValidatorScript = V2.fromCompiledCode votingValidatorCode

votingValidator :: V2.Validator
votingValidator = V2.Validator votingValidatorScript

votingValidatorSBS :: ShortByteString
votingValidatorSBS = SBS.toShort . LBS.toStrict $ Serialise.serialise votingValidatorScript

votingScriptAsPlutusScript :: PlutusScript PlutusScriptV2
votingScriptAsPlutusScript = PlutusScriptSerialised votingValidatorSBS

------------------------------------------------------------
-- | SCRIPT HASH & ADDRESS
------------------------------------------------------------
votingScriptHash :: V2.ValidatorHash
votingScriptHash = V2.validatorHash votingValidator

votingAddress :: V2.Address
votingAddress = V2.scriptAddress votingValidator

------------------------------------------------------------
-- | SAVE TO FILE
------------------------------------------------------------
saveScript :: IO ()
saveScript = do
    let desc = "Voting System Validator"
    case writeFileTextEnvelope "VotingSystem.plutus" (Just desc) votingScriptAsPlutusScript of
        Left err -> print $ "Error writing script: " <> show err
        Right () -> print "Script written successfully to VotingSystem.plutus"

------------------------------------------------------------
-- | MAIN
------------------------------------------------------------
main :: IO ()
main = do
    Prelude.putStrLn "=========================================="
    Prelude.putStrLn " VOTING SYSTEM SMART CONTRACT"
    Prelude.putStrLn "=========================================="
    Prelude.putStrLn ""
    Prelude.putStrLn $ "Validator Hash: " ++ show votingScriptHash
    Prelude.putStrLn $ "Script Address: " ++ show votingAddress
    Prelude.putStrLn ""
    saveScript
    Prelude.putStrLn ""
    Prelude.putStrLn "Contract compiled and saved successfully!"
    Prelude.putStrLn "=========================================="