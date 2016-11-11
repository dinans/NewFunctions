(ns tabular_functions.datatypes
  (
  :require
     [grafter.tabular :refer :all]
     [grafter.rdf :refer [s]]
     [grafter.rdf.protocols :refer [->Quad]]
     [grafter.rdf.templater :refer [graph]]
     [grafter.vocabularies.rdf :refer :all]
     [grafter.vocabularies.foaf :refer :all]
     [incanter.core :as inc]  
     [clj-time.format :refer :all]))


(defn- true-value? [x]  (if (or (= (clojure.string/trim (clojure.string/lower-case (str x))) "true") (and (re-matches #"[0-9.]+" (str x)) (not= (str x) "0"))) true false))


(defn- bool-value [x]  (if (or (= (clojure.string/trim (clojure.string/lower-case (str x))) "false")  (and (re-matches #"[0-9.]+" (str x)) (= (str x) "0")) ) false true))

(defn parse-date-eu [d]  
                               (.toDate (clj-time.format/parse (clj-time.format/formatter (clj-time.core/time-zone-for-offset 0) "dd/MM/yyyy"   "dd-MM-yyyy" "dd.MM.yyyy" ) d)))

(defn parse-date-us [d]  
                               (.toDate (clj-time.format/parse (clj-time.format/formatter (clj-time.core/time-zone-for-offset 0) "MM/dd/yyyy"   "MM-dd-yyyy" "MM.dd.yyyy" "yyyy-MM-dd" "yyyy.MM.dd" "yyyy/MM/dd" ) d)))

(defn date-validator [d] ( if (re-matches #"(?:(?:31(\/|-|\.)(?:0?[13578]|1[02]))\1|(?:(?:29|30)(\/|-|\.)(?:0?[1,3-9]|1[0-2])\2))(?:(?:1[6-9]|[2-9]\d)?\d{2})$|^(?:29(\/|-|\.)0?2\3(?:(?:(?:1[6-9]|[2-9]\d)?(?:0[48]|[2468][048]|[13579][26])|(?:(?:16|[2468][048]|[3579][26])00))))$|^(?:0?[1-9]|1\d|2[0-8])(\/|-|\.)(?:(?:0?[1-9])|(?:1[0-2]))\4(?:(?:1[6-9]|[2-9]\d)?\d{2})$" d) "eu"
                            ; mm.dd.yyyy mm/dd/yyyy mm-dd-yyyy
                            (if (re-matches #"(?:(?:(?:0?[13578]|1[02])(\/|-|\.)31)\1|(?:(?:0?[1,3-9]|1[0-2])(\/|-|\.)(?:29|30)\2))(?:(?:1[6-9]|[2-9]\d)?\d{2})$|^(?:0?2(\/|-|\.)29\3(?:(?:(?:1[6-9]|[2-9]\d)?(?:0[48]|[2468][048]|[13579][26])|(?:(?:16|[2468][048]|[3579][26])00))))$|^(?:(?:0?[1-9])|(?:1[0-2]))(\/|-|\.)(?:0?[1-9]|1\d|2[0-8])\4(?:(?:1[6-9]|[2-9]\d)?\d{2})$" d) "us"
                            ; yyyy-mm-dd yyyy/mm/dd yyyy.mm.dd
                            (if (re-matches #"(19|20)\d\d[- /.](0[1-9]|1[012])[- /.](0[1-9]|[12][0-9]|3[01])$" d) "us" nil)
                            )))



(defn to-int "" [x] 
  (let [m (.getDeclaredMethod clojure.lang.LispReader
                              "matchNumber"                                                                 
                              (into-array [String]))]
                              (do
                                (.setAccessible m true)    
                                (.invoke m clojure.lang.LispReader (into-array [x]))
                                )))

(defn is-numeric [x] (not (nil? (re-matches #"^((-*[0-9]+)|((-*[0-9]+)\.([0-9]+)))$" (str x)))))
(defn convert-literal "Converts given value to the specified datatype. Supported datatypes (with corresponding xsd types) :


                      |    Argument |     Datatype |                                        Value space |
                      |-------------+--------------+----------------------------------------------------|
                      |    \"byte\" |     xsd:byte |                                  -128…+127 (8 bit) | 
                      |   \"short\" |    xsd:short |                             -32768…+32767 (16 bit) |
                      |  \"double\" |   xsd:double |                      64-bit floating point numbers |
                      | \"decimal\" |  xsd:decimal |                Arbitrary-precision decimal numbers |
                      | \"integer\" |      xsd:int |                   -2147483648…+2147483647 (32 bit) |
                      |    \"long\" |     xsd:long | -9223372036854775808…+9223372036854775807 (64 bit) |
                      |   \"float\" |    xsd:float |                      32-bit floating point numbers |
                      | \"boolean\" |  xsd:boolean |                                        true, false | 
                      |    \"date\" | xsd:dateTime |                        Date and time with timezone |
                      |  \"string\" |   xsd:string |                                  Character strings |

                      
                  Optional keys: 
                  
                        :on-error -- specifies value, that should be used to replace non-valid arguments. By default function replaces all 
                                     non-valid values with 0 for all numeric types, \"false\" for data type boolean and \"31.12.2099\" for dates
                        
                        :on-empty -- specifies value, that should be used to replace empty(nil) arguments. By default function replaces all 
                                     empty values with 0 for all numeric types, \"false\" for data type boolean and \"31.12.2099\" for dates
                        
                        :lang-tag -- specifies a language tag used with string literals

                      Note: By default for conversions to data type boolean following values are converted to false:

                                   - false (as boolean);
                                   - nil;
                                   - \"\" (empty string);
                                   - \"false\" (as string);
                                   - \"0\" (as string);
                                   - 0 (as integer).                          
              
              " [x dtype  &{:keys [on-empty on-error lang-tag] :or {on-error false on-empty 0 lang-tag nil}}]
(let [f-check (case dtype
                 "byte" #(and  (is-numeric %) (< (Double/parseDouble (str %)) 128) (> (Double/parseDouble (str %)) -129))
                 "short" #(and  (is-numeric %) (< (Double/parseDouble (str %)) 32768) (> (Double/parseDouble (str %)) -32769))
                 "double" #(is-numeric %)
                 "decimal" #(is-numeric %)
                 "integer" #(is-numeric %)
                 "long" #(and  (is-numeric %) (< (Double/parseDouble (str %)) (Integer/parseInt "9223372036854775808")) (> (Double/parseDouble (str %)) (Integer/parseInt "9223372036854775809")))
                 "float" #(and  (is-numeric %) (float? (Double/parseDouble (str %))))
                  nil )

      f (case dtype 
              "byte" #(byte %)
              "short" #(short %)
              "double" #(double %)
              "decimal" #(bigdec %)
              "integer" #(int %)
              "long" #(long %)
              "float" #(float %)
              nil )
       default-date  (parse-date-eu "31.12.2099")
       arg (str x)
      ]
;numeric types
(case dtype
      ("byte" "short" "double" "decimal" "integer" "long" "float") (cond   (or (nil? x) (empty? arg)) (if (nil? (re-matches #"[0-9.]+" (str on-empty))) 0 (f (Double/parseDouble (str on-empty))))
                                        (not (f-check arg)) (if (not (f-check (str on-error)))
                                                                                             ;(nil? (re-matches #"[0-9.]+" (str on-error))) 
                                                                                             (f 0) 
                                                                                             (f (Double/parseDouble (str on-error))))
                                        ;(nil? (re-matches #"[0-9.]+" arg)) (if (nil? (re-matches #"[0-9.]+" (str on-error))) 0 (f (Double/parseDouble on-error)))
                                       ; :else (f (Double/parseDouble (apply str (re-seq #"[\d.]+" arg)))))
                                        :else (f (Double/parseDouble (str arg))))
       "boolean" (if (or (nil? x) (empty? (str x))) (true-value? on-empty) (true-value? arg))
      
       "date" (if (or (nil? x) (empty? (str x))) (if (date-validator (str on-empty)) (convert-literal on-empty "date") default-date)
                    (case (date-validator arg) 
                      "eu" (parse-date-eu arg)
                      "us" (parse-date-us arg)
                      (if (date-validator (str on-error)) (convert-literal on-error "date") default-date) ))
       (if (or (nil? x) (empty? (str x))) (if (nil? lang-tag) (s (str on-empty)) (s (str on-empty) lang-tag) ) (if (nil? lang-tag) (s x) (s x lang-tag)))
       )))

  

  

(defn integer-literal-new "Takes a string and converts it to datatype Integer.  Empty values are replaced with value given as a second parameter, 
  non-valid values are replaced with value given as a third parameter." 
  [n on-empty on-error] 
  (letfn [(str->int [x] (unchecked-int (Double/parseDouble (str x))))
          (clean-commas [x] (clojure.string/replace (str x) #"," ""))]
   (cond (or (nil? n) (empty? (clean-commas n))) (str->int on-empty) 
        (nil? (re-matches #"[0-9.]+" (clean-commas n))) (str->int on-error)
        :else (str->int (clean-commas n) ) ))) 

(defn double-literal "Takes a string and converts it to datatype Integer. Empty values are replaced with value given as a second parameter, 
  non-valid values are replaced with value given as a third parameter." 
  [n on-empty on-error] 
  (cond (or (nil? n) (empty? (str n))) on-empty 
        (nil? (re-matches #"[0-9.]+" (str n))) on-error 
        :else (Double/parseDouble (str n)) )) 

(defn date-literal "Takes a string and converts it to datatype Date in given format. Empty values are replaced with value given as a second 
  parameter, non-valid values are replaced with value given as a third parameter." 
;TODO: look at clj-time for conversions
  [n on-empty on-error date-format] (
.parse (java.text.SimpleDateFormat. date-format) n)                       )
