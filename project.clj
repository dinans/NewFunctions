(defproject grafterizer/tabular_functions "0.1.2"
  :description "Functions to perform tabular transformations for use in Grafter pipelines"
  :url "http://github.com/dinans/tabular_functions"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [grafter "0.5.0"]
                 [grafter/vocabularies "0.1.2"]
                 [org.slf4j/slf4j-jdk14 "1.7.5"]]
  :repl-options {:init (set! *print-length* 200)
                 :init-ns tabular_functions.pipeline }
 ;:main ^:skip-aot tabular_functions.core
 ;:source-paths ["src/tabular_functions"] 
 :target-path "target/%s"
  :jvm-opts ^:replace ["-server"
                       ;;"-XX:+AggressiveOpts"
                       ;;"-XX:+UseFastAccessorMethods"
                       ;;"-XX:+UseCompressedOops"
                       ;;"-Xmx4g"
                       ]
  
  :plugins [[lein-grafter "0.5.0"]
            [lein-codox "0.9.0"]]
  :min-lein-version "2.5.1"
  :profiles {:uberjar {:aot :all}}
  :codox {:output-path "doc/"})
