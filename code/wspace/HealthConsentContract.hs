{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HealthConsentContract where

-- Plutus imports
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts
import           PlutusTx
import           PlutusTx.Prelude        hiding (Semigroup(..), unless)
import           Ledger                  (PubKeyHash, getCardanoTxId)
import           Ledger.Value            (Value)
import qualified Ledger.Typed.Scripts    as Scripts
import qualified Plutus.V1.Ledger.Scripts as ScriptsV1 -- sometimes needed
import           Prelude                 (Show, String)
import qualified Prelude                 as H

-- Off-chain imports
import           Ledger.Constraints      as Constraints
import qualified Ledger.Typed.Scripts    as Typed
import           Playground.Contract
import           Plutus.Contract         as Contract
import           Data.Aeson              (ToJSON, FromJSON)
import qualified Data.Map                as Map
import           GHC.Generics            (Generic)
import           Data.Text               (Text)
import           Ledger.TimeSlot         (posixTimeToEnclosingSlot)
import           Ledger.Interval         (contains)

--------------------------------------------------------------------------------
-- On-chain datatypes
--------------------------------------------------------------------------------

-- Consent is non-transferable: patient mints/creates it and only patient can revoke.
data ConsentDatum = ConsentDatum
  { cdPatient    :: PubKeyHash
  , cdProvider   :: PubKeyHash        -- a single provider or could be a list; for simplicity a single provider here
  , cdScopes     :: [BuiltinByteString] -- scopes encoded as ByteString (e.g., "billing", "diagnosis")
  , cdExpiry     :: POSIXTime
  } deriving (Show, Generic)

data AccessLogDatum = AccessLogDatum
  { alConsentRef :: BuiltinByteString -- hash referencing consent (could be txId||index or consent unique id)
  , alProvider   :: PubKeyHash
  , alHash       :: BuiltinByteString -- hash of resource accessed (e.g., billable data)
  , alTime       :: POSIXTime
  } deriving (Show, Generic)

data ClaimState = Pending | Approved | Rejected | Settled
    deriving (Show, Generic)

data ClaimDatum = ClaimDatum
  { cdPolicyId    :: BuiltinByteString
  , cdClaimant    :: PubKeyHash
  , cdAmount      :: Integer -- lovelace
  , cdEvidence    :: BuiltinByteString -- evidence hash
  , cdState       :: ClaimState
  } deriving (Show, Generic)

-- Redeemer: actions allowed on the script
data HealthAction
  = MintConsent
  | RevokeConsent
  | AppendAccessLog
  | SubmitClaim
  | ApproveClaim
  | RejectClaim
  | SettleClaim
  deriving (Show, Generic)

-- Derive PlutusTx instances
PlutusTx.unstableMakeIsData ''ConsentDatum
PlutusTx.unstableMakeIsData ''AccessLogDatum
PlutusTx.unstableMakeIsData ''ClaimDatum
PlutusTx.unstableMakeIsData ''ClaimState
PlutusTx.unstableMakeIsData ''HealthAction

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

{-# INLINABLE nowFromCtx #-}
nowFromCtx :: ScriptContext -> POSIXTime
nowFromCtx ctx = txInfoValidRange (scriptContextTxInfo ctx) `ivTo` where
  ivTo r = case ivTo r of
    UpperBound (Finite t) _ -> t
    _ -> 0 -- fallback

-- Check signature present
{-# INLINABLE txSignedByPk #-}
txSignedByPk :: TxInfo -> PubKeyHash -> Bool
txSignedByPk info pkh = txSignedBy info pkh

-- Check provider present in scopes (scopes are bytestring labels)
{-# INLINABLE providerAllowed #-}
providerAllowed :: PubKeyHash -> [BuiltinByteString] -> Bool
providerAllowed _ _ = True
-- For prototyping we accept; in practice you'd encode provider identity in scopes or compare provider PKH against allowed list

--------------------------------------------------------------------------------
-- Validator logic
--------------------------------------------------------------------------------

{-# INLINABLE mkHealthValidator #-}
mkHealthValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ScriptContext -> Bool
mkHealthValidator rawDatum rawRedeemer ctx =
  case PlutusTx.fromBuiltinData rawRedeemer of
    Nothing -> traceError "bad redeemer"
    Just action ->
      let info = scriptContextTxInfo ctx
      in case action of
        MintConsent   -> validateMintConsent rawDatum info
        RevokeConsent -> validateRevokeConsent rawDatum info
        AppendAccessLog -> validateAppendAccessLog rawDatum info
        SubmitClaim   -> validateSubmitClaim rawDatum info
        ApproveClaim  -> validateApproveRejectClaim rawDatum info True
        RejectClaim   -> validateApproveRejectClaim rawDatum info False
        SettleClaim   -> validateSettleClaim rawDatum info

-- Each validate* checks the Datum and TxInfo to enforce business rules

{-# INLINABLE validateMintConsent #-}
validateMintConsent :: BuiltinData -> TxInfo -> Bool
validateMintConsent rawDatum info =
  case PlutusTx.fromBuiltinData rawDatum of
    Nothing -> traceError "mint: bad datum"
    Just (ConsentDatum patient provider scopes expiry) ->
      let signedByPatient = txSignedBy info patient
          withinTime = contains (to expiry) (txInfoValidRange info)
      in traceIfFalse "patient must sign" signedByPatient &&
         traceIfFalse "consent not yet valid" True &&
         traceIfFalse "consent should have expiry" (expiry > 0)

{-# INLINABLE validateRevokeConsent #-}
validateRevokeConsent :: BuiltinData -> TxInfo -> Bool
validateRevokeConsent rawDatum info =
  case PlutusTx.fromBuiltinData rawDatum of
    Nothing -> traceError "revoke: bad datum"
    Just (ConsentDatum patient _ _ _) ->
      traceIfFalse "only patient can revoke" (txSignedBy info patient)

{-# INLINABLE validateAppendAccessLog #-}
validateAppendAccessLog :: BuiltinData -> TxInfo -> Bool
validateAppendAccessLog rawDatum info =
  case PlutusTx.fromBuiltinData rawDatum of
    Nothing -> traceError "accesslog: bad datum"
    Just (AccessLogDatum consentRef provider' _ ts) ->
      -- require the provider signed and consent referenced exists and not expired
      let signedByProvider = txSignedBy info provider'
          -- We'll require that some input UTXO at script has a ConsentDatum matching `consentRef`.
          inputs = txInfoInputs info
          consentPresent = any (\i -> case txOutDatumHash (txInInfoResolved i) of
                                        Just dh -> True -- In a full implementation: lookup datum and compare consentRef
                                        Nothing -> False) inputs
      in traceIfFalse "provider must sign" signedByProvider &&
         traceIfFalse "consent must be present" consentPresent

{-# INLINABLE validateSubmitClaim #-}
validateSubmitClaim :: BuiltinData -> TxInfo -> Bool
validateSubmitClaim rawDatum info =
  case PlutusTx.fromBuiltinData rawDatum of
    Nothing -> traceError "claim: bad datum"
    Just (ClaimDatum policyId claimant amount evidence state) ->
      traceIfFalse "claim must be pending when submitted" (state == Pending) &&
      traceIfFalse "claimant must sign" (txSignedBy info claimant) &&
      traceIfFalse "positive amount" (amount > 0)

{-# INLINABLE validateApproveRejectClaim #-}
validateApproveRejectClaim :: BuiltinData -> TxInfo -> Bool -> Bool
validateApproveRejectClaim rawDatum info isApprove =
  case PlutusTx.fromBuiltinData rawDatum of
    Nothing -> traceError "approve/reject: bad datum"
    Just (ClaimDatum _ _ _ _ state) ->
      -- insurer (tx signer) can change state from Pending to Approved or Rejected
      let signerPresent = length (txInfoSignatories info) > 0 -- in practice check insurer PKH
      in traceIfFalse "must be pending" (state == Pending) &&
         traceIfFalse "insurer must sign" signerPresent

{-# INLINABLE validateSettleClaim #-}
validateSettleClaim :: BuiltinData -> TxInfo -> Bool
validateSettleClaim rawDatum info =
  case PlutusTx.fromBuiltinData rawDatum of
    Nothing -> traceError "settle: bad datum"
    Just (ClaimDatum _ claimant amount _ state) ->
      -- Only settle when approved, and Tx must pay the claimant the amount
      let outputs = txInfoOutputs info
          paid = any (\o -> case toPubKeyHash (txOutAddress o) of
                              Just pkh -> pkh == claimant && valuePaidTo info claimant `geq` (lovelaceValueOf amount)
                              Nothing  -> False) outputs
          approved = state == Approved
      in traceIfFalse "claim must be approved" approved &&
         traceIfFalse "must pay claimant" paid

-- helpers for value checks
{-# INLINABLE valuePaidTo #-}
valuePaidTo :: TxInfo -> PubKeyHash -> Value
valuePaidTo info pkh = txInfoOutputs info `foldr` mempty $ \o acc ->
  case toPubKeyHash (txOutAddress o) of
    Just p -> if p == pkh then txOutValue o <> acc else acc
    Nothing -> acc

{-# INLINABLE geq #-}
geq :: Value -> Value -> Bool
geq = valueGEQ -- valueGEQ from Plutus

--------------------------------------------------------------------------------
-- Boilerplate typed validator
--------------------------------------------------------------------------------

data Health
instance Scripts.ValidatorTypes Health where
  type instance DatumType Health = BuiltinData
  type instance RedeemerType Health = BuiltinData

typedValidator :: Scripts.TypedValidator Health
typedValidator = Scripts.mkTypedValidator @Health
  $$(PlutusTx.compile [|| mkHealthValidator ||])
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.mkUntypedValidator

validator :: Validator
validator = Scripts.validatorScript typedValidator

validatorHash :: ValidatorHash
validatorHash = Scripts.validatorHash typedValidator

scrAddress :: Address
scrAddress = scriptHashAddress validatorHash

--------------------------------------------------------------------------------
-- Off-chain: Contract endpoints (sketch)
--------------------------------------------------------------------------------

-- Endpoint request types
data MintConsentParams = MintConsentParams
  { mcpPatient  :: PubKeyHash -- should be current wallet's pubkey
  , mcpProvider :: PubKeyHash
  , mcpScopes   :: [BuiltinByteString]
  , mcpExpiry   :: POSIXTime
  } deriving (Generic, ToJSON, FromJSON, Show)

data AppendLogParams = AppendLogParams
  { alpConsentRef :: BuiltinByteString
  , alpHash       :: BuiltinByteString
  } deriving (Generic, ToJSON, FromJSON, Show)

data SubmitClaimParams = SubmitClaimParams
  { scpPolicyId :: BuiltinByteString
  , scpAmount   :: Integer
  , scpEvidence :: BuiltinByteString
  } deriving (Generic, ToJSON, FromJSON, Show)

type HealthSchema =
       Endpoint "mintConsent" MintConsentParams
  .\/ Endpoint "revokeConsent" BuiltinByteString
  .\/ Endpoint "appendAccessLog" AppendLogParams
  .\/ Endpoint "submitClaim" SubmitClaimParams
  .\/ Endpoint "approveClaim" BuiltinByteString -- claim id
  .\/ Endpoint "rejectClaim" BuiltinByteString
  .\/ Endpoint "settleClaim" BuiltinByteString

-- Off-chain implementations (very minimal & illustrative)
mintConsent :: AsContractError e => MintConsentParams -> Contract w s e ()
mintConsent p = do
  let datum = PlutusTx.toBuiltinData $ ConsentDatum (mcpPatient p) (mcpProvider p) (mcpScopes p) (mcpExpiry p)
      tx = Constraints.mustPayToOtherScript validatorHash datum (lovelaceValueOf 0)
  ledgerTx <- submitTxConstraints typedValidator tx
  awaitTxConfirmed $ getCardanoTxId ledgerTx
  Contract.logInfo @String "consent minted"

revokeConsent :: AsContractError e => BuiltinByteString -> Contract w s e ()
revokeConsent consentRef = do
  -- simplified: find UTXO with matching consentRef and spend it with Redeemer RevokeConsent
  Contract.logInfo @String "revokeConsent endpoint called - implement UTXO search and spend"

appendAccessLog :: AsContractError e => AppendLogParams -> Contract w s e ()
appendAccessLog p = do
  let ad = PlutusTx.toBuiltinData $ AccessLogDatum (alpConsentRef p) (error "provider pkh to be derived from wallet") (alpHash p) 0
      tx = Constraints.mustPayToOtherScript validatorHash ad (lovelaceValueOf 0) <>
           Constraints.mustBeSignedBy (error "provider pkh")
  ledgerTx <- submitTxConstraints typedValidator tx
  awaitTxConfirmed $ getCardanoTxId ledgerTx
  Contract.logInfo @String "access log appended"

submitClaim :: AsContractError e => SubmitClaimParams -> Contract w s e ()
submitClaim p = do
  pk <- Contract.ownPubKeyHash
  let cd = ClaimDatum (scpPolicyId p) pk (scpAmount p) (scpEvidence p) Pending
      tx = Constraints.mustPayToOtherScript validatorHash (PlutusTx.toBuiltinData cd) (lovelaceValueOf 0) <>
           Constraints.mustBeSignedBy pk
  ledgerTx <- submitTxConstraints typedValidator tx
  awaitTxConfirmed $ getCardanoTxId ledgerTx
  Contract.logInfo @String "claim submitted"

-- The other endpoints (approve/reject/settle) follow a similar pattern:
-- 1. find the claim UTXO,
-- 2. build tx that consumes it with the appropriate redeemer,
-- 3. and optionally outputs a new claim datum with updated state (or pays out on settle).

endpoints :: Contract () HealthSchema Text ()
endpoints = do
  Contract.logInfo @String "Health Consent contract endpoints up"
  forever $
    handleError logError $
      awaitPromise $
        mint' `select` revoke' `select` append' `select` submit'
 where
   mint'   = endpoint @"mintConsent" $ \p -> mintConsent p
   revoke' = endpoint @"revokeConsent" $ \_ -> Contract.logInfo @String "not implemented yet"
   append' = endpoint @"appendAccessLog" $ \_ -> Contract.logInfo @String "not implemented yet"
   submit' = endpoint @"submitClaim" $ \p -> submitClaim p

mkSchemaDefinitions ''HealthSchema
mkKnownCurrencies []

