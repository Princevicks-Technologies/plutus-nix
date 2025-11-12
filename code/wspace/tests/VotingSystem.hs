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

module VotingSystem where

import           PlutusTx
import           PlutusTx.Prelude hiding (Semigroup(..), unless)
import           Ledger
import           Ledger.Typed.Scripts as Scripts
import           Ledger.Value as Value
import           Prelude (IO)
import qualified Prelude as P
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LBS
import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import           Codec.Serialise (serialise)

--------------------------------------------------------------------------------
-- | On-chain voting logic
--------------------------------------------------------------------------------

data Vote = Yes | No
PlutusTx.unstableMakeIsData ''Vote

{-# INLINABLE mkValidator #-}
mkValidator :: Vote -> Vote -> ScriptContext -> Bool
mkValidator redeemer datum _ =
    traceIfFalse "Vote does not match expected value" (redeemer == datum)

data Voting
instance Scripts.ValidatorTypes Voting where
    type instance RedeemerType Voting = Vote
    type instance DatumType Voting = Vote

typedValidator :: Scripts.TypedValidator Voting
typedValidator = Scripts.mkTypedValidator @Voting
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Vote @Vote

validator :: Validator
validator = Scripts.validatorScript typedValidator

validatorHash :: ValidatorHash
validatorHash = Scripts.validatorHash typedValidator

votingAddress :: Address
votingAddress = scriptAddress validator

--------------------------------------------------------------------------------
-- | Serialisation helpers
--------------------------------------------------------------------------------

votingValidatorSBS :: SBS.ShortByteString
votingValidatorSBS = SBS.toShort . LBS.toStrict $ serialise validator

votingScriptAsPlutusScript :: PlutusScript PlutusScriptV2
votingScriptAsPlutusScript = PlutusScriptSerialised votingValidatorSBS

--------------------------------------------------------------------------------
-- | Main for testing and output
--------------------------------------------------------------------------------

main :: IO ()
main = do
    P.putStrLn "✅ Voting System Smart Contract Compiled Successfully!"
    P.putStrLn $ "Validator Hash: " P.++ P.show validatorHash
    P.putStrLn $ "Script Address: " P.++ P.show votingAddress
