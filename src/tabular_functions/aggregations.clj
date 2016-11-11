(ns tabular_functions.aggregations
  (
  :require
     [grafter.tabular :refer :all]
     [grafter.rdf :refer [s]]
     [grafter.rdf.protocols :refer [->Quad]]
     [grafter.rdf.templater :refer [graph]]
     [grafter.vocabularies.rdf :refer :all]
     [grafter.vocabularies.foaf :refer :all]
     [tabular_functions.datatypes :refer [is-numeric date-validator convert-literal]]
     [incanter.core :as inc]  ))

(defn- get-datatype [x y] (if (and (date-validator (str x)) (date-validator (str y))) "date" (if (and (is-numeric (str x)) (is-numeric (str y))) "double" nil)))

(defn MIN [arg1 arg2] (letfn [
  (get-min [a b] (if (< (compare a b) 0) arg1 arg2))
  (convert [x] (if (get-datatype arg1 arg2) (convert-literal (str x) (get-datatype arg1 arg2)) (str x)))]
                       (get-min (convert arg1) (convert arg2))
                       ))
(defn MAX [arg1 arg2] (letfn [
  (get-max [a b] (if (> (compare a b) 0) arg1 arg2))
  (convert [x] (if (get-datatype arg1 arg2) (convert-literal (str x) (get-datatype arg1 arg2)) (str x)))]
                       (get-max (convert arg1) (convert arg2))
                       ))
(defn SUM [& args] (apply + (map #(Double/parseDouble (str %)) args)))
(defn COUNT [& args] (count (into [] args)))
(defn AVG [& args] (/ (apply SUM args) (apply COUNT args)))
(defn COUNT-DISTINCT [& args] (count (distinct (into [] args))))
(defn median [& args] (let [sorted (sort args)
                            cnt (count sorted)
                            halfway (quot cnt 2)]
                        (println args)
                        ;(if (odd? cnt)
                          
                        ;  (nth sorted halfway) 
                        ;  (let [bottom (dec halfway)
                        ;        bottom-val (nth sorted bottom)
                        ;        top-val (nth sorted halfway)]
                        ;    (AVG bottom-val top-val)))
                        ))
(defn MEDIAN [& args] (println args
                       ;apply median (map #(Double/parseDouble (str %)) args)
                       ))
(defn MODE [& args] (let [numeric-args (map #(Double/parseDouble (str %)) args)
                          freqs (apply frequencies numeric-args)
                          occurrences (group-by val freqs)
                          modes (last (sort occurrences))
                          modes (->> modes
                                     val
                                     (map key))]
                     modes))
(defn STD-DEV [& args] (let [numeric-args (map #(Double/parseDouble (str %)) args)
                             avg (apply AVG numeric-args)
                             squares (for [x numeric-args]
                                        (let [x-avg (- x avg)]
                                          (* x-avg x-avg)))
                             total (apply count numeric-args)]
                           (-> (/ (apply + squares)
                                  (- total 1))
                               (Math/sqrt))))

(defn MERGE [separator & args] (clojure.string/join separator (distinct (into [] (map str args)))))
