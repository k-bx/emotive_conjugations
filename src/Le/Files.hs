module Le.Files where

import Conduit
import Le.Import
import Le.S3Loc
import Network.AWS as AWS
import Network.AWS.S3 as S3 hiding (bucket)

s3download :: S3Loc -> FilePath -> AWS ()
s3download (S3Loc r b k) to_ =
  AWS.within
    r
    (AWS.send (S3.getObject b k) >>= sinkGetBody (sinkFileCautious to_))

sinkGetBody ::
  MonadIO m =>
  ConduitM ByteString Void (ResourceT IO) a ->
  GetObjectResponse ->
  m a
sinkGetBody to_ gr = AWS.sinkBody (gr ^. gorsBody) to_
