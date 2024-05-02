{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module PlutusExample.PlutusVersion3.AlwaysSucceeds
  ( alwaysSucceedsScript
  , alwaysSucceedsScriptShortBs
  ) where

import Prelude hiding (($))

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS

import PlutusLedgerApi.V3 as PV3
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), unless, (.))

{-# INLINABLE mkValidator #-}
mkValidator :: PV3.BuiltinData -> PV3.BuiltinData -> PV3.BuiltinData -> ()
mkValidator _ _ _ = ()

validator :: PV3.Validator
validator = PV3.mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

script :: PV3.Script
script = PV3.unValidatorScript validator

alwaysSucceedsScriptShortBs :: SBS.ShortByteString
alwaysSucceedsScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

alwaysSucceedsScript :: PlutusScript PlutusScriptV1
alwaysSucceedsScript = PlutusScriptSerialised alwaysSucceedsScriptShortBs

