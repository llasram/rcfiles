{:user
 {:plugins [[lein-pprint "1.1.1"]
            [lein-ancient "0.6.8"]
            [lein-difftest "2.0.0"]]}
 :repl
 {:plugins [[cider/cider-nrepl "0.11.0"]]
  :dependencies [[org.clojure/tools.nrepl "0.2.12"]
                 [alembic "0.3.2"]
                 [criterium "0.4.3"]]
  :jvm-opts ["-server" "-Xmx16g"]}}
