module Le.Types where

import Control.Newtype
import qualified Data.Aeson as J
import qualified Data.String.Class as S
import qualified Database.Persist.Postgresql as P
import qualified Network.URI
import RIO
import qualified Servant.API
import qualified Web.PathPieces

newtype JsonList a
  = JsonList [a]
  deriving (Show)

unJsonList :: JsonList a -> [a]
unJsonList (JsonList xs) = xs

instance (J.ToJSON a, J.FromJSON a) => P.PersistField (JsonList a) where
  toPersistValue = P.toPersistValueJSON . unJsonList

  fromPersistValue x =
    case x of
      P.PersistText "" -> Right (JsonList [])
      _ -> fmap JsonList (P.fromPersistValueJSON x)

instance (J.ToJSON a, J.FromJSON a) => P.PersistFieldSql (JsonList a) where
  sqlType _ = P.SqlString -- sqlUtf8Text

instance Newtype (JsonList a) [a] where
  pack = JsonList

  unpack (JsonList x) = x

newtype LoginTokenVal
  = LoginTokenVal Text
  deriving
    ( Show,
      Read,
      Eq,
      Ord,
      P.PersistField,
      P.PersistFieldSql,
      Web.PathPieces.PathPiece,
      Servant.API.ToHttpApiData,
      Servant.API.FromHttpApiData,
      J.ToJSON,
      J.FromJSON
    )

instance Newtype LoginTokenVal Text

newtype PersistentAbsoluteURI
  = PersistentAbsoluteURI Network.URI.URI
  deriving (Show, Eq)

instance Newtype PersistentAbsoluteURI Network.URI.URI

instance P.PersistField PersistentAbsoluteURI where
  toPersistValue x = P.toPersistValue (tshow (unpack x))

  fromPersistValue v = do
    t <- P.fromPersistValue v
    let mUri = Network.URI.parseAbsoluteURI t
    maybe
      (Left ("Failed to parse an absolute URI: " <> (S.toText t)))
      (return . pack)
      mUri

instance P.PersistFieldSql PersistentAbsoluteURI where
  sqlType _ = P.SqlString
