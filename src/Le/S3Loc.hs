module Le.S3Loc where

import Data.Aeson
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Data.List
import qualified Data.String.Class as S
import qualified Data.Text as T
import Le.Import
import Network.AWS.Data.Text as AWS
import Network.AWS.S3 (BucketName (..), ObjectKey (..), Region (..))
import qualified Network.HTTP.Types.URI
import qualified Network.URI

-- | Fully qualified S3 Location.
-- It is hard to peform most operations on a bucket without knowing its region.
data S3Loc
  = S3Loc Region BucketName ObjectKey
  deriving (Show, Eq, Ord, Generic)

instance IsString S3Loc where
  fromString = either error id . both
    where
      both x = eitherDecode @S3Loc (S.fromString x) <|> parseS3Url x

instance FromJSON S3Loc where
  parseJSON o@(J.Object _) =
    flip (withObject "S3Loc") o \v ->
      S3Loc
        <$> do
          reg <- v .: "region"
          either fail pure (AWS.fromText reg)
        <*> (BucketName <$> v .: "bucket")
        <*> (ObjectKey <$> v .: "key")
  parseJSON (J.String x) =
    either fail pure (parseS3Url x)
  parseJSON invalid =
    J.typeMismatch "String or proper Object" invalid

instance ToJSON S3Loc where
  toJSON (S3Loc r (BucketName b) (ObjectKey k)) =
    object
      [ "bucket" .= b,
        "key" .= k,
        "region" .= AWS.toText r
      ]

toCliS3Url :: S3Loc -> Text
toCliS3Url (S3Loc _ (BucketName b) (ObjectKey k)) = "s3://" <> b <> "/" <> k

toS3Url :: S3Loc -> Text
toS3Url (S3Loc r (BucketName b) (ObjectKey k)) = "s3-" <> AWS.toText r <> "://" <> b <> "/" <> k

-- | See 'Le.StubData' for examples
parseS3Url :: S.ConvString s => s -> Either String S3Loc
parseS3Url uri =
  case Network.URI.parseAbsoluteURI (S.toString uri) of
    Just (URI "https:" (Just (URIAuth _ ('s' : '3' : '.' : host) _)) ('/' : bk) _ _) ->
      case break (== '/') bk of
        (bucket@(_ : _), '/' : path) ->
          case let (region, _) = break (== '.') host
                in AWS.fromText (S.toText region) of
            Right r ->
              Right (S3Loc r (BucketName (S.fromString bucket)) (ObjectKey (S.fromStrictByteString (unescapePercent (S.toStrictByteString path)))))
            Left e ->
              Left e
        _ ->
          Left "expected url in form https://s3.eu-central-1.amazonaws.com/bucket/key"
    Just (URI "https:" (Just (URIAuth _ host _)) ('/' : key) _ _) ->
      -- default to us-east-1
      -- https://panopteo-asr-jsonoutput.s3.amazonaws.com/result_tk_plus_df0821to_tk0820_new/rttmm
      case Data.List.reverse (T.split (== '.') (S.toText host)) of
        "com" : "amazonaws" : "s3" : b ->
          let bucket = T.intercalate "." (Data.List.reverse b)
           in Right (S3Loc NorthVirginia (BucketName bucket) (ObjectKey (S.fromStrictByteString (unescapePercent (S.toStrictByteString key)))))
        _ ->
          Left ("parseS3Url: wrong us-east-1 url: " <> S.toString uri)
    Just (URI "s3:" (Just (URIAuth _ b _)) ('/' : k) _ _) ->
      -- TODO: warn: "'s3:' urls are insufficient, include region in the scheme like 's3-eu-central:'"
      Right (S3Loc Frankfurt (BucketName (S.fromString b)) (ObjectKey (S.fromString k)))
    Just (URI ('s' : '3' : '-' : region) (Just (URIAuth _ b _)) ('/' : k) _ _) ->
      case AWS.fromText (S.toText (Data.List.init region)) of
        Right r ->
          Right (S3Loc r (BucketName (S.fromString b)) (ObjectKey (S.fromString k)))
        Left e ->
          Left e
    Just x ->
      Left ("parseS3Url: wrong url: " <> show x)
    Nothing ->
      Left ("parseS3Url: bad url: " <> S.toString uri)
  where
    unescapePercent = Network.HTTP.Types.URI.urlDecode True
