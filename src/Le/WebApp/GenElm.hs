{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Le.WebApp.GenElm where

import qualified Data.String.Class as S
import qualified Data.Text as T
import Elm.Module
import Elm.TyRep (ETCon (..))
import qualified Le.ApiTypes as AT
import Le.Import
import qualified Le.Python
import Le.Routes
import Servant
import Servant.API.Generic
import Servant.Elm
import Servant.Elm.Internal.Foreign
import Servant.Foreign

instance
  HasForeign LangElm EType a =>
  HasForeign LangElm EType (AuthProtect "cookie-auth" :> a)
  where
  type Foreign EType (AuthProtect "cookie-auth" :> a) = Foreign EType a

  foreignFor l ft _api req = foreignFor l ft (Proxy :: Proxy a) req

moduleDefs :: [DefineElm]
moduleDefs =
  [ DefineElm (Proxy :: Proxy AT.NoOp),
    DefineElm (Proxy :: Proxy AT.ArticleShort),
    DefineElm (Proxy :: Proxy AT.Article),
    DefineElm (Proxy :: Proxy AT.ArticlePlease),
    DefineElm (Proxy :: Proxy AT.ArticlePleaseBig),
    DefineElm (Proxy :: Proxy AT.NamedEntityGroup),
    DefineElm (Proxy :: Proxy Le.Python.CmdSpacyNerResEnt),
    DefineElm (Proxy :: Proxy Le.Python.CmdSpacyPosResEnt),
    DefineElm (Proxy :: Proxy (AT.Paginated Text)),
    DefineElm (Proxy :: Proxy AT.QueueAddForm)
  ]

replacements :: [(Text, Text)]
replacements =
  [ ("(Key Article)", "ArticleId"),
    ("(Key ArticleNp)", "ArticleNpId"),
    ("(Key ArticlePlease)", "ArticlePleaseId"),
    ("(Key ArticlePleaseBig)", "ArticlePleaseBigId"),
    ( "jsonDec(Tuple2 Text Text)",
      "jsonDecTuple2 Json.Decode.string Json.Decode.string"
    ),
    ("Http.expectJson", "leExpectJson"),
    ("Http.Error", "Error"),
    ( "Err e -> toMsg (Err e)",
      "Err e -> toMsg (Err {httpError=e,formErrors=Dict.empty,errors=[]})"
    )
  ]

intAliases :: [String]
intAliases =
  mkIntAliases
    [ "IntUTCTime",
      "IntZonedTime",
      "Milliseconds",
      "ArticleId",
      "ArticlePleaseId",
      "ArticlePleaseBigId"
    ]

stringAliases :: [String]
stringAliases =
  mkStringAliases
    [ "DayString",
      "TimeZoneName",
      "TimeZoneUILabel",
      "TZLabel",
      "S3Loc"
    ]

floatAliases :: [String]
floatAliases = mkFloatAliases ["Scientific"]

elmHeader :: String
elmHeader =
  [q|module Le.Api exposing (..)

{-| WARNING! This file is auto-generated, don't edit it manually.

See `make generate-elm` and `GenElm.hs` for details.
-}

import Http
import Json.Decode exposing (Value)
import Json.Encode
import Json.Helpers exposing (required, fnullable, maybeEncode, decodeSumUnaries)
import Dict exposing (Dict)
import Url.Builder

type alias Text = String
jsonDecText = Json.Decode.string

type alias Tuple2 a b = { t2f1 : a, t2f2 : b }

-- tuple2 a b = { t2f1 = a, t2f2 = b }

jsonEncTuple2 : (a -> Value) -> (b -> Value) -> Tuple2 a b -> Value
jsonEncTuple2  enc1 enc2 tpl =
   Json.Encode.object
   [ ("1", enc1 tpl.t2f1)
   , ("2", enc2 tpl.t2f2)
   ]

jsonDecTuple2 : Json.Decode.Decoder dec1 -> Json.Decode.Decoder dec2 -> Json.Decode.Decoder (Tuple2 dec1 dec2)
jsonDecTuple2 dec1 dec2 =
   Json.Decode.succeed (\pv1 pv2 -> {t2f1 = pv1, t2f2 = pv2})
   |> required "1" dec1
   |> required "2" dec2

jsonPair : Json.Decode.Decoder a -> Json.Decode.Decoder b -> Json.Decode.Decoder ( a, b )
jsonPair a b =
    Json.Decode.andThen
        (\xs ->
            case xs of
                [] ->
                    Json.Decode.fail "Expecting a list of two elements"

                x :: y :: [] ->
                    case ( Json.Decode.decodeValue a x, Json.Decode.decodeValue b y ) of
                        ( Ok av, Ok bv ) ->
                            Json.Decode.succeed ( av, bv )

                        _ ->
                            Json.Decode.fail "Error while decoding individual pair fields"

                _ ->
                    Json.Decode.fail "Expecting a list of two elements"
        )
        (Json.Decode.list Json.Decode.value)

type alias Error =
    { httpError : Http.Error
    , formErrors : Dict String String
    , errors : List String
    }

leExpectJson : (Result Error a -> msg) -> Json.Decode.Decoder a -> Http.Expect msg
leExpectJson toMsg decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err { httpError = Http.BadUrl url, formErrors = Dict.empty, errors = [] }

                Http.Timeout_ ->
                    Err { httpError = Http.Timeout, formErrors = Dict.empty, errors = [] }

                Http.NetworkError_ ->
                    Err { httpError = Http.NetworkError, formErrors = Dict.empty, errors = [] }

                Http.BadStatus_ metadata body ->
                    case metadata.statusCode of
                        400 ->
                            case Json.Decode.decodeString (Json.Decode.list (jsonPair Json.Decode.string Json.Decode.string)) body of
                                Err decodeErr ->
                                    Err
                                        { httpError = Http.BadStatus metadata.statusCode
                                        , errors = [ "Error while decoding a back-end response: " ++ Json.Decode.errorToString decodeErr ]
                                        , formErrors = Dict.empty
                                        }

                                Ok vs ->
                                    Err
                                        { httpError = Http.BadStatus metadata.statusCode
                                        , errors = []
                                        , formErrors = Dict.fromList vs
                                        }

                        _ ->
                            Err { httpError = Http.BadStatus metadata.statusCode, formErrors = Dict.empty, errors = ["There was a back-end error. Try again or contact administrators"] }

                Http.GoodStatus_ metadata body ->
                    case Json.Decode.decodeString decoder body of
                        Ok value ->
                            Ok value

                        Err err ->
                            Err { httpError = Http.BadBody (Json.Decode.errorToString err), formErrors = Dict.empty, errors = [] }

|]

elmToString' :: EType -> Text
elmToString' argType' =
  case argType' of
    ETyCon (ETCon "Bool") ->
      "(\\value -> if value then \"true\" else \"false\")"
    ETyCon (ETCon "Float") -> "String.fromFloat"
    ETyCon (ETCon "Char") -> "String.fromChar"
    ETyApp (ETyCon (ETCon "Maybe")) v ->
      "(Maybe.map " <> defaultElmToString v <> " >> Maybe.withDefault \"\")"
    ETyCon (ETCon "S3Loc") -> "identity"
    _ -> "String.fromInt"

generateElmNew :: IO ()
generateElmNew = do
  S.putStrLn elmHeader
  let defs = makeModuleContent moduleDefs
  S.putStrLn defs
  forM_ intAliases $ \t -> do S.putStrLn t
  forM_ stringAliases $ \t -> do S.putStrLn t
  forM_ floatAliases $ \t -> do S.putStrLn t
  let elmOpts = defElmOptions {elmToString = elmToString'}
  forM_ (generateElmForAPIWith elmOpts (Proxy :: Proxy (ToServantApi JsonAPI))) $ \t -> do
    S.putStrLn (doReplaces t)
    S.putStrLn ("" :: Text)

doReplaces :: Text -> Text
doReplaces s =
  foldl' (\curr (from, to') -> T.replace from to' curr) s replacements

mkIntAlias :: String -> [String]
mkIntAlias x =
  [ "type alias " <> x <> " = Int",
    "jsonDec" <> x <> " = Json.Decode.int",
    "jsonEnc" <> x <> " = Json.Encode.int"
  ]

mkIntAliases :: Foldable t => t String -> [String]
mkIntAliases xs = concatMap mkIntAlias xs

mkStringAlias :: String -> [String]
mkStringAlias x =
  [ "type alias " <> x <> " = String",
    "jsonDec" <> x <> " = Json.Decode.string",
    "jsonEnc" <> x <> " = Json.Encode.string"
  ]

mkStringAliases :: Foldable t => t String -> [String]
mkStringAliases xs = concatMap mkStringAlias xs

mkFloatAlias :: String -> [String]
mkFloatAlias x =
  [ "type alias " <> x <> " = Float",
    "jsonDec" <> x <> " = Json.Decode.float",
    "jsonEnc" <> x <> " = Json.Encode.float"
  ]

mkFloatAliases :: Foldable t => t String -> [String]
mkFloatAliases xs = concatMap mkFloatAlias xs

run :: IO ()
run = generateElmNew
