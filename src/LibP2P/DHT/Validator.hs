-- | Record validation for the Kademlia DHT.
--
-- Per specs/kad-dht (Entry validation), records must be validated on
-- two occasions: values retrieved in a GET_VALUE query and values
-- received in a PUT_VALUE query before storing them locally.
--
-- Record keys have the form @/namespace/path@; validation dispatches on
-- the namespace, mirroring go-libp2p's @record.NamespacedValidator@.
-- The built-in @/pk/@ validator binds the key to the value: the path
-- must be the multihash (Peer ID) of the serialized public key carried
-- in the value.
module LibP2P.DHT.Validator
  ( -- * Validator interface
    Validator (..)
    -- * Built-in validators
  , namespacedValidator
  , pkValidator
  , defaultValidator
    -- * Key helpers
  , splitRecordKey
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import LibP2P.Crypto.PeerId (fromPublicKey, peerIdBytes)
import LibP2P.Crypto.Protobuf (decodePublicKey)

-- | Validator interface for record validation.
--
-- 'valValidate' checks that a value is well-formed for its key and
-- returns an error for records that must not be stored or served.
-- 'valSelect' picks the index of the best value among conflicting
-- candidates for the same key (used for GET_VALUE conflict resolution).
data Validator = Validator
  { valValidate :: ByteString -> ByteString -> Either String ()
  , valSelect   :: ByteString -> [ByteString] -> Either String Int
  }

-- | Split a record key of the form @/namespace/path@ into
-- (namespace, path). The path is raw bytes (for @/pk/@ it is a binary
-- multihash), so only the two leading separators are interpreted.
splitRecordKey :: ByteString -> Either String (ByteString, ByteString)
splitRecordKey key = case BS.uncons key of
  Just (0x2F, rest) ->
    let (ns, pathWithSlash) = BS.break (== 0x2F) rest
    in case BS.uncons pathWithSlash of
         Just (0x2F, path) -> Right (ns, path)
         _ -> Left "invalid record key: missing namespace separator"
  _ -> Left "invalid record key: missing leading '/'"

-- | Dispatch validation by key namespace. Keys without a registered
-- namespace are rejected, matching go-libp2p's namespaced validator
-- (\"invalid record keytype\").
namespacedValidator :: Map ByteString Validator -> Validator
namespacedValidator validators = Validator
  { valValidate = \key value -> do
      v <- validatorFor key
      valValidate v key value
  , valSelect = \key values -> do
      v <- validatorFor key
      valSelect v key values
  }
  where
    validatorFor key = do
      (ns, _) <- splitRecordKey key
      case Map.lookup ns validators of
        Nothing -> Left ("invalid record keytype: " ++ BSC.unpack ns)
        Just v -> Right v

-- | Validator for the @/pk/@ namespace: the value must be a serialized
-- PublicKey protobuf whose derived Peer ID equals the multihash in the
-- key path. Public keys never conflict, so 'valSelect' keeps the first
-- candidate (go-libp2p's @record.PublicKeyValidator@ does the same).
pkValidator :: Validator
pkValidator = Validator
  { valValidate = \key value -> do
      (_, mh) <- splitRecordKey key
      pub <- decodePublicKey value
      let derived = peerIdBytes (fromPublicKey pub)
      if derived == mh
        then Right ()
        else Left "public key does not match record key"
  , valSelect = \_ values ->
      if null values
        then Left "no values to select from"
        else Right 0
  }

-- | The default validator set: @/pk/@ records only. Additional
-- namespaces can be registered by building a custom
-- 'namespacedValidator' and storing it in the DHT node.
defaultValidator :: Validator
defaultValidator = namespacedValidator (Map.fromList [(BSC.pack "pk", pkValidator)])
