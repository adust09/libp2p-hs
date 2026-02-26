-- | Wire format for multistream-select.
--
-- Every message is: <varint-length><UTF-8 payload>\n
-- The length includes the trailing newline byte.
module Network.LibP2P.MultistreamSelect.Wire
  ( encodeMessage
  , decodeMessage
  , multistreamHeader
  , naMessage
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.LibP2P.Core.Varint (decodeUvarint, encodeUvarint)

-- | The multistream-select protocol header.
multistreamHeader :: Text
multistreamHeader = "/multistream/1.0.0"

-- | The "not available" response.
naMessage :: Text
naMessage = "na"

-- | Encode a multistream-select message (protocol ID or command).
-- Produces: <varint(len+1)><UTF-8 encoded text><0x0a>
encodeMessage :: Text -> ByteString
encodeMessage msg =
  let payload = TE.encodeUtf8 msg <> BS.singleton 0x0a
      len = fromIntegral (BS.length payload)
   in encodeUvarint len <> payload

-- | Decode a multistream-select message from bytes.
-- Returns the text content (without newline) and remaining bytes.
decodeMessage :: ByteString -> Either String (Text, ByteString)
decodeMessage bs = do
  (len, rest1) <- decodeUvarint bs
  let msgLen = fromIntegral len :: Int
  if BS.length rest1 < msgLen
    then Left "decodeMessage: not enough bytes"
    else
      let (payload, rest2) = BS.splitAt msgLen rest1
       in if BS.null payload || BS.last payload /= 0x0a
            then Left "decodeMessage: message does not end with newline"
            else
              let text = TE.decodeUtf8 (BS.init payload)
               in Right (text, rest2)
  where
    -- BS.init is safe here because we checked non-null above
