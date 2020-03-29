let Mode = < Master | Worker >

in  { cfgHttpPort = Some 6666
    , cfgMode = Mode.Worker
    , cfgDataDir = "/home/ubuntu/conj_data"
    , cfgPsqlConnString =
        "host=localhost port=5432 user=postgres dbname=conj_worker password=password"
    }
