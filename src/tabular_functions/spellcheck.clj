(ns tabular_functions.spellcheck
  (
  :require
     [grafter.tabular :refer :all]
     [grafter.rdf :refer [s]]
     [grafter.rdf.protocols :refer [->Quad]]
     [grafter.rdf.templater :refer [graph]]
     [grafter.vocabularies.rdf :refer :all]
     [grafter.vocabularies.foaf :refer :all]
     [tabular_functions.aggregations :refer :all]
     [incanter.core :as inc]  ))

(def wordlist 
  (set (map clojure.string/trim (clojure.string/split-lines (slurp "resources/wordsEn.txt")))))

(defn incorrect? 
  [word]
  (not (contains? wordlist word)))

(defn build-rows [dataset colname]   (map #(filter not-empty  (-> (get % colname)
                                                                  (clojure.string/lower-case)
                                                                  (clojure.string/replace #"[^a-z\s]" "")
                                                                  (clojure.string/split #"[\W]")))  
                                          (:rows dataset)))

(defn check-spelling-in-colum 
  ([dataset colname] 
   (filter #(not-empty (second %)) (apply hash-map (interleave (range) (map #(filter incorrect? %) 
                                                                            (build-rows dataset colname))))))
  ([dataset colname lang] ()))
