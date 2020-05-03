{- Contains default config without secrets. should be symlinked near the "secret" dhall files for typechecking to work
-}

let Mode = < Master | Worker >

in  { Mode = Mode
    , def =
      { cfgHttpPort = Some 8080
      , cfgMode = Mode.Master
      , cfgDataDir = "/home/kb/tmp/emotive_conjugations/data"
      , cfgPsqlConnString =
          "host=localhost port=5432 user=postgres dbname=conj password=password"
      -- , cfgRemotePsqlConnString =
      --     "host=localhost port=5442 user=postgres dbname=conj password=password"
      , cfgPythonWebapp = "http://localhost:8082"
      , cfgMailgunDomain = "mg.emotive-conjugations.app"
      , cfgMailgunApiKey = "somekey"
      , cfgFacebookAppId = "750050712196580"
      , cfgFacebookAppSecret = "somesecret"
      -- , cfgFacebookAppToken = "sometoken"
      , cfgGoogleOauthClientId = "313436810105-et1hm4ic03bao6ru2npk2i5qjkrca5am.apps.googleusercontent.com"
      , cfgGoogleOauthClientSecret = "somesecret"
      }
    }
