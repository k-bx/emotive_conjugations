let Mode = < Master | Worker >

in  { cfgHttpPort = Some 6666
    , cfgMode = Mode.Worker
    , cfgDataDir = "/home/ubuntu/conj_data"
    }

