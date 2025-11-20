{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}

module Main where

import Prelude (IO, String, FilePath, putStrLn, (<>))
import qualified Prelude as P
import qualified Data.Text as T

-- Plutus core
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V1.Ledger.Interval as Interval
import Plutus.V1.Ledger.Value (valueOf, adaSymbol, adaToken)
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins as Builtins


-- Serialization
import qualified Codec.Serialise as Serialise
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString       as BS

-- Cardano API (for Bech32 address)
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS

--------------------------------------------------------------------------------
-- On-chain datatypes: Ticket and RevenueSplitter
--------------------------------------------------------------------------------

-- Ticket datum (stored at ticket script UTxO)
data Ticket = Ticket
    { tSeat        :: Builtins.BuiltinByteString
    , tEventId     :: Builtins.BuiltinByteString
    , tTransferCap :: Integer            -- max number of resales allowed
    , tKYCRequired :: Bool               -- whether transfers require off-chain KYC
    , tOwner       :: PubKeyHash         -- current owner / signer for certain ops
    , tResaleCount :: Integer            -- how many times this ticket was transferred
    , tDeadline    :: POSIXTime          -- transfer deadline (e.g. event start)
    }
PlutusTx.unstableMakeIsData ''Ticket

-- RevenueSplitter datum (stored at revenue UTxO)
data RevenueSplitter = RevenueSplitter
    { rsEventId       :: Builtins.BuiltinByteString
    , rsBeneficiaries :: [(PubKeyHash, Integer)] -- (addr, percentage). Percentages must sum to 100.
    , rsCollected     :: Integer                  -- total collected (lovelace) tracked off-chain or on-chain
    }
PlutusTx.unstableMakeIsData ''RevenueSplitter

-- Redeemers for ticket script
data TicketRedeemer
    = MintTicket Integer           -- amount paid in primary sale (lovelace)
    | TransferTicket PubKeyHash    -- new owner PubKeyHash
    | GateCheck                    -- on-scan gate check (no state change)
PlutusTx.unstableMakeIsData ''TicketRedeemer

-- Redeemers for revenue script
data RevenueRedeemer = ClaimPayout
PlutusTx.unstableMakeIsData ''RevenueRedeemer

--------------------------------------------------------------------------------
-- Shared helpers
--------------------------------------------------------------------------------

{-# INLINABLE valuePaidToPkh #-}
-- how much ADA (lovelace) was paid to a PubKeyHash in this Tx
valuePaidToPkh :: TxInfo -> PubKeyHash -> Integer
valuePaidToPkh info pkh = valueOf (valuePaidTo info pkh) adaSymbol adaToken

{-# INLINABLE totalPerc #-}
totalPerc :: [(PubKeyHash,Integer)] -> Integer
totalPerc xs = foldl (\acc (_,p) -> acc + p) 0 xs

{-# INLINABLE percentAmount #-}
percentAmount :: Integer -> Integer -> Integer
percentAmount total pct = (total `multiply` pct) `divide` 100

{-# INLINABLE nowBefore #-}
nowBefore :: POSIXTime -> TxInfo -> Bool
nowBefore t info = Interval.contains (to t) (txInfoValidRange info)

{-# INLINABLE nowAfter #-}
nowAfter :: POSIXTime -> TxInfo -> Bool
nowAfter t info = Interval.contains (from t) (txInfoValidRange info)

{-# INLINABLE kycPlaceholder #-}
-- On-chain we cannot perform real KYC. Off-chain must enforce KYC when tKYCRequired==True.
kycPlaceholder :: Bool -> PubKeyHash -> Bool
kycPlaceholder _ _ = True

--------------------------------------------------------------------------------
-- Ticket validator
--------------------------------------------------------------------------------

{-# INLINABLE mkTicketValidator #-}
mkTicketValidator :: Ticket -> TicketRedeemer -> ScriptContext -> Bool
mkTicketValidator t red ctx =
    case red of
        MintTicket paid     -> validateMint paid
        TransferTicket newP -> validateTransfer newP
        GateCheck           -> validateGate
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedBy :: PubKeyHash -> Bool
    signedBy = txSignedBy info

    validateMint :: Integer -> Bool
    validateMint paid =
        -- Primary mint must be signed by the organizer (we assume tOwner is organiser during mint)
        signedBy (tOwner t) &&
        paid `geq` 0 -- permissive placeholder; off-chain should require exact price & minting policy

    validateTransfer :: PubKeyHash -> Bool
    validateTransfer newOwner =
        -- current owner must sign, resale cap not exceeded, optional KYC, and must be before deadline
        signedBy (tOwner t) &&
        (tResaleCount t `lt` tTransferCap t) &&
        kycPlaceholder (tKYCRequired t) newOwner &&
        nowBefore (tDeadline t) info

    validateGate :: Bool
    validateGate =
        -- Gate check: ensure ticket not expired and optionally other checks; signed by scanner operator is optional
        nowBefore (tDeadline t) info

-- Boilerplate wrapping
{-# INLINABLE wrappedTicket #-}
wrappedTicket :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedTicket d r c =
    let datum    = PlutusTx.unsafeFromBuiltinData @Ticket d
        redeemer = PlutusTx.unsafeFromBuiltinData @TicketRedeemer r
        ctx      = PlutusTx.unsafeFromBuiltinData @ScriptContext c
    in if mkTicketValidator datum redeemer ctx
       then ()
       else PlutusTx.Prelude.traceError "Ticket validation failed"

ticketValidator :: Validator
ticketValidator = mkValidatorScript $$(PlutusTx.compile [|| wrappedTicket ||])

--------------------------------------------------------------------------------
-- RevenueSplitter validator (enforces payouts meet percentages)
--------------------------------------------------------------------------------

{-# INLINABLE mkRevenueValidator #-}
mkRevenueValidator :: RevenueSplitter -> RevenueRedeemer -> ScriptContext -> Bool
mkRevenueValidator rs red ctx =
    case red of
        ClaimPayout -> validateClaim
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- The caller must be signed by an authorized organizer / beneficiary.
    -- For simplicity require one of the beneficiaries to sign (first beneficiary) OR any signer (you can tighten this).
    firstBeneficiary :: Maybe PubKeyHash
    firstBeneficiary = case rsBeneficiaries rs of
                         (pk,_) : _ -> Just pk
                         _          -> Nothing

    signedByAuthorized :: Bool
    signedByAuthorized = case firstBeneficiary of
                           Just pk -> txSignedBy info pk
                           Nothing -> False

    totalCollected :: Integer
    totalCollected = rsCollected rs

    validateClaim :: Bool
    validateClaim =
      traceIfFalse "unauthorized caller" signedByAuthorized &&
      traceIfFalse "invalid percentages" (totalPerc (rsBeneficiaries rs) == 100) &&
      traceIfFalse "no revenue to split" (totalCollected > 0) &&
      -- For each beneficiary, ensure tx pays at least their percentage of totalCollected
      foldl (\acc (pk,pct) -> acc && (valuePaidToPkh info pk `geq` percentAmount totalCollected pct)) True (rsBeneficiaries rs)

-- Boilerplate wrapping
{-# INLINABLE wrappedRevenue #-}
wrappedRevenue :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrappedRevenue d r c =
    let datum    = PlutusTx.unsafeFromBuiltinData @RevenueSplitter d
        redeemer = PlutusTx.unsafeFromBuiltinData @RevenueRedeemer r
        ctx      = PlutusTx.unsafeFromBuiltinData @ScriptContext c
    in if mkRevenueValidator datum redeemer ctx
       then ()
       else PlutusTx.Prelude.traceError "Revenue validation failed"

revenueValidator :: Validator
revenueValidator = mkValidatorScript $$(PlutusTx.compile [|| wrappedRevenue ||])

--------------------------------------------------------------------------------
-- Helpers to compute Plutus ValidatorHash (same method used in your escrow example)
--------------------------------------------------------------------------------

{-# INLINABLE plutusValidatorHash #-}
plutusValidatorHash :: PlutusV2.Validator -> PlutusV2.ValidatorHash
plutusValidatorHash val =
    let bytes    = Serialise.serialise val
        short    = SBS.toShort (LBS.toStrict bytes)
        strictBS = SBS.fromShort short
        builtin  = Builtins.toBuiltin strictBS
    in PlutusV2.ValidatorHash builtin

-- Build Plutus script address (on-chain Address) from validator
plutusScriptAddress :: Validator -> Address
plutusScriptAddress val =
    Address (ScriptCredential (plutusValidatorHash val)) Nothing

-- Convert to Bech32 (Cardano API)
toBech32ScriptAddress :: C.NetworkId -> Validator -> String
toBech32ScriptAddress network val =
    let serialised = SBS.toShort . LBS.toStrict $ Serialise.serialise val
        plutusScript :: C.PlutusScript C.PlutusScriptV2
        plutusScript = CS.PlutusScriptSerialised serialised

        scriptHash = C.hashScript (C.PlutusScript C.PlutusScriptV2 plutusScript)

        shelleyAddr :: C.AddressInEra C.BabbageEra
        shelleyAddr =
            C.makeShelleyAddressInEra
                network
                (C.PaymentCredentialByScript scriptHash)
                C.NoStakeAddress
    in T.unpack (C.serialiseAddress shelleyAddr)

--------------------------------------------------------------------------------
-- Write validators to files
--------------------------------------------------------------------------------

writePlutusScript :: FilePath -> Validator -> IO ()
writePlutusScript path val = do
    let s = Serialise.serialise val
    BS.writeFile path (LBS.toStrict s)
    putStrLn $ "Wrote plutus script to: " <> path

--------------------------------------------------------------------------------
-- Main: write both ticket and revenue scripts and print addresses/hashes
--------------------------------------------------------------------------------

main :: IO ()
main = do
    let net = C.Testnet (C.NetworkMagic 1)

    -- write ticket validator
    writePlutusScript "ticketing.plutus" ticketValidator
    let tvh  = plutusValidatorHash ticketValidator
        taddr = plutusScriptAddress ticketValidator
        tbech = toBech32ScriptAddress net ticketValidator

    putStrLn "\n--- Ticket Validator ---"
    putStrLn $ "Plutus ValidatorHash (ticket): " <> P.show tvh
    putStrLn $ "On-chain Address (ticket):     " <> P.show taddr
    putStrLn $ "Bech32 Address (ticket):       " <> tbech

    -- write revenue validator
    writePlutusScript "revenue.plutus" revenueValidator
    let rvh  = plutusValidatorHash revenueValidator
        raddr = plutusScriptAddress revenueValidator
        rbech = toBech32ScriptAddress net revenueValidator

    putStrLn "\n--- Revenue Validator ---"
    putStrLn $ "Plutus ValidatorHash (revenue): " <> P.show rvh
    putStrLn $ "On-chain Address (revenue):     " <> P.show raddr
    putStrLn $ "Bech32 Address (revenue):       " <> rbech

    putStrLn "\nDone: ticketing + revenue scripts written."

