(ns tabular_functions.stats
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

(defn compute-stats "Collects general statistics for the dataset" [ds]
  () )
