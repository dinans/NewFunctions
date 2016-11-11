(ns tabular_functions.pipeline
  (
  :require
     [grafter.tabular :refer :all]
     [grafter.rdf :refer [s]]
     [grafter.sequences :refer :all]
     [grafter.rdf.protocols :refer [->Quad]]
     [grafter.rdf.templater :refer [graph]]
     [grafter.vocabularies.rdf :refer :all]
     [grafter.vocabularies.foaf :refer :all]
     [incanter.core :as inc]  
     [tabular_functions.aggregations :refer :all]
     [tabular_functions.datatypes :refer :all]

     )
  )
(comment 
(defn fill-cells
  "Fills cells in a dataset with a given value. Value can be specified either as static value or as value contained in cell with coordinates {:row x :cell y}.
 Usage example:
 
  | a | b | c | d | e |
  |---+---+---+---+---|
  | a | b | b | c | c |
  | 2 | 2 | 2 | 2 | 2 |
  | 2 | 2 | 2 | 2 | 2 |
  | 2 | 2 | 2 | 2 | 2 |
  | 0 | 1 | 8 | 5 | 0 |


  `(fill-cells dataset "2" {:b_row 1, :b_col 0, :e_row 3, :e_col 3}) ; =>`

                         | a | b | c | d | e |
                         |---+---+---+---+---|
                         | a | b | b | c | c |
                         | 2 | 2 | 2 | 2 | 5 |
                         | 2 | 2 | 2 | 2 | 1 |
                         | 2 | 2 | 2 | 2 | 2 |
                         | 0 | 1 | 8 | 5 | 0 |
  "
  [dataset value cells-to-fill]
  (let [val (if (map? value) (incanter.core/sel dataset :rows (get value :row) :cols (get value :col))
                             value)
        b_row (get cells-to-fill :b_row)
        e_row (get cells-to-fill :e_row)
        b_col (get cells-to-fill :b_col)
        e_col (get cells-to-fill :e_col)
        colnames (column-names dataset)
        sort-row (fn [row] (let [order-map (apply hash-map (interleave colnames (range)))]
                                                   (conj 
                                                     (sorted-map-by #(compare (order-map %1) (order-map %2)))
                                                     (select-keys row colnames))))
        eods (count (:rows dataset))
        preceding-rows (-> dataset 
                           (take-rows b_row)
                           (:rows)
                         )
        filled-rows (incanter.core/conj-rows (->> (rows dataset (range b_row (inc e_row)))
                         (:rows)
                         (map (fn [row] 
                                (loop [cnt b_col rowmap (sort-row row)]
                                  (if (> cnt e_col)
                                    rowmap
                                    (recur (inc cnt)
                                           (assoc rowmap (nth colnames cnt) val))))))
                         ))
                        
        subsequent-rows (-> dataset 
                            (rows [(inc e_row) eods])
                            (:rows))] 


          (-> (make-dataset (concat preceding-rows filled-rows subsequent-rows)
                            colnames)
              (with-meta (meta dataset)))
         ))

)
  (defn ^:no-doc add-filename-to-column [ds destination-column] (let [fname (:grafter.tabular/data-source (meta ds))] (add-column ds destination-column fname)))
  (defn ^:no-doc spread-values
   [ds]
   (let [fill-row (fn [row] (let [colnames (column-names ds)
                                  sort-row (fn [row] (let [order-map (apply hash-map (interleave colnames (range)))]
                                                       (conj 
                                                         (sorted-map-by #(compare (order-map %1) (order-map %2)))
                                                         (select-keys row (column-names ds)))))]
                               (apply hash-map (interleave colnames (grafter.sequences/fill-when (vals (sort-row row)))))))]

    (-> (make-dataset (map fill-row (:rows ds)) (column-names ds))(with-meta (meta ds))(apply-columns (into {} (for [x (column-names ds)]  (assoc {} x (symbol "grafter.sequences" "fill-when"))))))
        )
   )
(defn fill-row-when 
  "Same as fill-when but for row. Can be either one row or range from-to.
  Usage example:
  | a | b | c | d | e |
  |---+---+---+---+---|
  | a | b | b | c | c |
  | 1 | 2 | 3 | 4 | 5 |
  | 2 |   | 3 |   |   |
  | 8 | 1 | 1 | 0 | 2 |
  | 0 | 1 | 8 | 5 | 0 |

  ´(fill-row-when dataset 2) ;=> `

  | a | b | c | d | e |
  |---+---+---+---+---|
  | a | b | b | c | c |
  | 1 | 2 | 3 | 4 | 5 |
  | 2 | 2 | 3 | 3 | 3 |
  | 8 | 1 | 1 | 0 | 2 |
  | 0 | 1 | 8 | 5 | 0 |
  "
  ([ds rownum]
  (let [colnames (column-names ds)
        eods (count (:rows ds))
        sorted-row (-> ds (:rows) (nth rownum))
        filled-values (fill-when (map sorted-row colnames))
        filled-row (list (apply hash-map (interleave colnames filled-values)))
        preceding-rows  (-> ds (rows (range 0  rownum)) (:rows))
        subsequent-rows (-> ds (rows (range (inc rownum)  eods)) (:rows))
        ]
      (-> (make-dataset 
            (concat preceding-rows filled-row subsequent-rows) 
            colnames)
                    (with-meta (meta ds)))))
  ([ds from to]
   (letfn [(build-filled-row [rownum] 
             (let [colnames (column-names ds)
                   sorted-row (-> ds (:rows) (nth rownum))
                   filled-values (fill-when (map sorted-row colnames))]
               (list (apply hash-map (interleave colnames filled-values)))))]
     (let [colnames (column-names ds)
           eods (count (:rows ds))
           preceding-rows  (-> ds (rows (range 0  from)) (:rows))
           subsequent-rows (-> ds (rows (range (inc to)  eods)) (:rows))
           filled-rows (loop [cnt from rowlist ()]
                         (if (> cnt to)
                           rowlist
                           (recur (inc cnt)
                                  (concat rowlist (build-filled-row cnt)))))]
      (-> (make-dataset 
            (concat preceding-rows filled-rows subsequent-rows) 
            colnames)
                    (with-meta (meta ds)))))))


(defn ^:no-doc stringify-all-columns [ds] 
  (let [colnames (column-names ds)
        colcount (count colnames)]
    (loop [cnt 0
           data ds]
      (if (= cnt colcount)
        data
        (recur (inc cnt)
               (mapc data {(nth colnames cnt) str}))))))
 
(defn parser [string attr] 
  (let [values (-> string 
                   (str) 
                   (clojure.string/replace #":LEEFTIJDEN_EN_GEBOORTEJAREN__" "") 
                   (clojure.string/split #"_"))]
    (cond (= attr "age_min") (if (or (= (first values) "beneden")
                                     (= (first values) "TOTAAL")) "0" (first values)) 
          (= attr "age_max") (cond (= (first values) "beneden") (nth values 1)
                                   (or (= (first values) "TOTAAL") (= (first values) "daarboven")) "999"
                                   :else (nth values 2))
          :else "")))


(defn ^:no-doc string-as-keyword [s]  (-> (str s) clojure.string/trim (clojure.string/replace "(" "-") (clojure.string/replace ")" "") (clojure.string/replace " " "_") (clojure.string/replace "," "-") (clojure.string/replace "." "") (clojure.string/replace "/" "-") (clojure.string/replace "---" "-") (clojure.string/replace "--" "-") (clojure.string/replace ":" "") (clojure.string/replace "\n" "_") (clojure.string/replace "\"" "")))

(defn add-row "Inserts new row into a dataset. Two options are available:
  1. Takes a dataset and vector containing field values and appends new row to the end of a dataset, e.g.
          
          Given original dataset
          
              |:col1|:col2|:col3|
              |-----|-----|-----|
              | 1   | 2   | 3   |
              | 4   | 5   | 6   |
              | 7   | 8   | 9   |

          function returns the following result:
       
              `(add-row dataset [10 11 12]) ; =>`

      
              |:col1|:col2|:col3|
              |-----|-----|-----|
              | 1   | 2   | 3   |
              | 4   | 5   | 6   |
              | 7   | 8   | 9   |
              | 10  | 11  | 12  | 

  2. Takes a dataset, row index and vector containing field values and inserts new row at the specified position. 
     If position index is negative or greater then total number of rows in a dataset, the new row will be apended to a dataset.

          Given original dataset
          
              |:col1|:col2|:col3|
              |-----|-----|-----|
              | 1   | 2   | 3   |
              | 4   | 5   | 6   |
              | 7   | 8   | 9   |

          function returns the following result:
       
              `(add-row dataset 1 [10 11 12]) ; =>`

      
              |:col1|:col2|:col3|
              |-----|-----|-----|
              | 1   | 2   | 3   |
              | 10  | 11  | 12  | 
              | 4   | 5   | 6   |
              | 7   | 8   | 9   |
              
              
              `(add-row dataset -2 [10 11 12]) ; =>`

      
              |:col1|:col2|:col3|
              |-----|-----|-----|
              | 10  | 11  | 12  | 
              | 1   | 2   | 3   |
              | 4   | 5   | 6   |
              | 7   | 8   | 9   |


    For both options if number of parameters denoting field values is less than current number of columns in a dataset, 
    lacking values for columns will remain empty. If number of parameters denoting field values is greater than number 
    of columns in a dataset, rest of the values will be discarded." 
 ( [dataset [& values]]
    (-> (make-dataset (:rows (incanter.core/conj-rows dataset values)) (column-names dataset))(with-meta (meta dataset))))
 ( [dataset position [& values]]
    ( if (or (< position 0) (>= position (count (:rows dataset)))) (add-row dataset [values])
    (-> (make-dataset (:rows (incanter.core/conj-rows (take-rows dataset position ) 
                                                      values 
                                                      (rows dataset (range position (count (:rows dataset))))
                                                      )) 
                      (column-names dataset))
        (with-meta (meta dataset))
        )))
)
   
(defn index-duplicate-values 
  ""
  [dataset rownum]
  (let [colnames (column-names dataset)
        sorted-values (-> dataset
                          (:rows)
                          (nth rownum)
                          (map colnames))   
        duplicate-values-frequencies (filter (fn [arg] (> (val arg) 1)) (frequencies sorted-values))
        duplicate-values  (map (fn [arg] (first arg)) duplicate-values-frequencies)
        values-indexed-map (into {} (map-indexed hash-map sorted-values))
        duplicate-valnumbers  (apply concat (map (fn [arg] (filter (comp #{arg} values-indexed-map) (keys values-indexed-map))) duplicate-values))
        indexed-duplicate-values (apply concat (map (fn [arg] (map str  (repeat (second arg) (first arg)) (range 0 (second arg)))) duplicate-values-frequencies))
        correction-map (zipmap duplicate-valnumbers indexed-duplicate-values)
        corrected-values (vals (into (sorted-map) (merge values-indexed-map  correction-map)))
        ]
        (-> dataset (rows (concat (range 0 rownum)(drop (inc rownum) (range)))) (add-row rownum corrected-values)
        )
    ))

(defn shift-soe-recs [dataset]
  (letfn [(is-numeric [x] (not (nil? (re-matches #"^((-*[0-9]+)|((-*[0-9]+)\.([0-9]+)))$" (str x)))))
          (drop-nth [n coll] (concat (take n coll) (drop (inc n) coll)))
          (fix-row [row] (let [colnames (column-names dataset)](->> (map row colnames)
                                                                    (drop-nth 1)
                                                                    (reverse)
                                                                    (apply conj '(""))
                                                                    (zipmap colnames)
                                                                    )))]
  (let [colnames (column-names dataset)
        wrong-rows (-> (grep dataset (complement is-numeric) [:EIENDOMSNR]) (:rows))
        correct-rows (-> (grep dataset is-numeric [:EIENDOMSNR]) (:rows))
        fixed-rows (loop [fixed-rows () cnt 0]
                     (if (= cnt (count wrong-rows))
                         fixed-rows
                         (recur (conj fixed-rows (fix-row (nth wrong-rows cnt))) (inc cnt) )))  ]
    (-> (make-dataset (:rows (incanter.core/conj-rows correct-rows
                                                      fixed-rows))
                      (column-names dataset))
        (with-meta (meta dataset)))
    ;fixed-rows
    )))
(defn mapr [dataset rownum f]
      (let [colnames (column-names dataset)
            eods (count (:rows dataset))
            row (-> dataset
                    (:rows)
                    (nth rownum))
            mapped-row (into {} (for [[k v] row] [k (f v)]))
            order-map (apply hash-map (interleave colnames (range)))
            sorted-mapped-row (conj (sorted-map-by #(compare (order-map %1) (order-map %2))) (select-keys mapped-row colnames))
            preceding-rows (take-rows dataset rownum) 
            subsequent-rows (rows dataset (range (inc rownum) eods))
]
    ( -> (make-dataset (:rows (incanter.core/conj-rows preceding-rows
                                                       sorted-mapped-row 
                                                       subsequent-rows))
                       (column-names dataset))
         (with-meta (meta dataset)))))

(defn mapr1 [dataset rownum f]
      (let [colnames (column-names dataset)
            eods (count (:rows dataset))
            row (-> dataset
                    (:rows)
                    (nth rownum))
            mapped-row (into {} (for [[k v] row] [k (f v)]))
            order-map (apply hash-map (interleave colnames (range)))
            sorted-mapped-row (conj (sorted-map-by (fn [arg1 arg2] (compare (order-map arg1) (order-map arg2)))) (select-keys mapped-row colnames))
            preceding-rows (take-rows dataset rownum) 
            subsequent-rows (rows dataset (range (inc rownum) eods))
]
    ( -> (make-dataset (:rows (incanter.core/conj-rows preceding-rows
                                                       sorted-mapped-row 
                                                       subsequent-rows))
                       (column-names dataset))
         (with-meta (meta dataset)))))
(defn shift-row "Changes row's position inside a dataset. Two options are available:
  1. Takes a dataset and row index and moves this row to the end of a dataset, data rows with indices greater than specified index 
     will be moved one position up e.g.
          
          Given original dataset
          
              |:col1|:col2|:col3|
              |-----|-----|-----|
              | 1   | 2   | 3   |
              | 4   | 5   | 6   |
              | 7   | 8   | 9   |
              | 10  | 11  | 12  | 

          function returns the following result:
       
              `(shift-row dataset 1) ; =>`

      
              |:col1|:col2|:col3|
              |-----|-----|-----|
              | 1   | 2   | 3   |
              | 7   | 8   | 9   |
              | 10  | 11  | 12  | 
              | 4   | 5   | 6   |

  2. Takes a dataset and two row indices and moves row from index #1 to index #2.  
     Other rows will be shifted appropriately.

          Given original dataset
          
              |:col1|:col2|:col3|
              |-----|-----|-----|
              | 1   | 2   | 3   |
              | 4   | 5   | 6   |
              | 7   | 8   | 9   |
              | 10  | 11  | 12  | 

          function returns the following result:
       
              `(shift-row dataset 1 3) ; =>`

      
              |:col1|:col2|:col3|
              |-----|-----|-----|
              | 1   | 2   | 3   |
              | 7   | 8   | 9   |
              | 10  | 11  | 12  | 
              | 4   | 5   | 6   |
                
   " 
 ( [dataset position-from]
  (let [preceding-rows (take-rows dataset position-from) 
        subsequent-rows (rows dataset (range (inc position-from) (count (:rows dataset))))
        row-to-shift (rows dataset [position-from])]
    ( -> (make-dataset (:rows (incanter.core/conj-rows preceding-rows 
                                                       subsequent-rows
                                                       row-to-shift ))
                     (column-names dataset))
       (with-meta (meta dataset))

  ))
 )
 ( [dataset position-from position-to]
  (let [lower (min (inc position-from) position-to)
        upper (max (inc position-to) position-from)
        eods (count (:rows dataset))
        preceding-rows (take-rows dataset (min position-from position-to))
        rows-between (rows dataset (range lower upper))
        row-to-shift (rows dataset [position-from])
        middle-rows (if (< position-from position-to) (incanter.core/conj-rows rows-between row-to-shift) (incanter.core/conj-rows row-to-shift rows-between) )
        subsequent-rows (rows dataset (range (max (inc position-from) (inc position-to)) eods))
         ] 
         (if (= position-from position-to) dataset
          (-> (make-dataset (:rows (incanter.core/conj-rows preceding-rows
                                                           middle-rows
                                                           subsequent-rows))

                           (column-names dataset))
             (with-meta (meta dataset))
             )
         ))
         )
  )
 
(defn- get-comparator [sorttype] 
  (let [f (cond 
            (= sorttype :ascalpha)       #(compare (str %1) (str %2)) 
            (= sorttype :descalpha)      #(compare (str %2) (str %1)) 
            (= sorttype :ascnum)         #(<  (Double/parseDouble (str %1))  (Double/parseDouble (str %2)))  
            (= sorttype :descnum)        #(>  (Double/parseDouble (str %1))  (Double/parseDouble (str %2))) 
            (= sorttype :asclen)         #(< (count (str %1)) (count (str %2))) 
            (= sorttype :desclen)        #(> (count (str %1)) (count (str %2)))
            (= sorttype :ascdate)        #(compare (.parse (java.text.SimpleDateFormat. "dd.MM.yyyy") (str %1))
                                                   (.parse (java.text.SimpleDateFormat. "dd.MM.yyyy") (str %2)))
            (= sorttype :descdate)       #(compare (.parse (java.text.SimpleDateFormat. "dd.MM.yyyy") (str %2))
                                                   (.parse (java.text.SimpleDateFormat. "dd.MM.yyyy") (str %1)))
            :else                        #(compare %1 %2)
            )
               ]
   f )
 ) 
    

(defn sort-dataset "Sorts dataset by given column names in given order. Column names and types of sorting are given in a vector. 
  Sorting priority is defined by order of column name -- sorting type pair. Sorting by multiple columns works as follows: 
  if several rows have equal columns(first in the vector of given columns) according to the given comparator type, these rows 
  will be sorted by second column and second comparator, if both first and second are equal, sorting will be performed by the third column 
  and third comparator etc.                 
  
  Type of comparator used for sorting is defined as one of following:

                   - :ascalpha, :descalpha for alphabetical sorting (in ascending and descending order correspondingly);
                   - :ascnum, :descnum for numerical sorting;
                   - :asclen, :desclen for sorting by field length;
                   - :ascdate, :descdate for sorting dates

    Examples:
                   
             Given original dataset
   
                   |  :a |                 :b | :c |         :d |
                   |-----+--------------------+----+------------|
                   |   2 |             string |  1 | 01.01.2015 |
                   | 111 |             string |  3 | 03.11.2015 |
                   |  44 |      longer string |  9 | 03.03.2013 |
                   |   3 | the longest string |  6 | 25.12.2015 |


             calling function with different parameters results in following datasets:

                `(sort-dataset dataset [{:a :ascalpha}]) ; sort by column :a in ascending alphabetical order =>`
                   

                   |  :a |                 :b | :c |         :d |
                   |-----+--------------------+----+------------|
                   | 111 |             string |  3 | 03.11.2015 |
                   |   2 |             string |  1 | 01.01.2015 |
                   |   3 | the longest string |  6 | 25.12.2015 |
                   |  44 |      longer string |  9 | 03.03.2013 |



               `(sort-dataset dataset [{:a :descalpha}]) ; sort by column :a in descending alphabetical order =>`

                   |  :a |                 :b | :c |         :d |
                   |-----+--------------------+----+------------|
                   | 111 |             string |  3 | 03.11.2015 |
                   |   2 |             string |  1 | 01.01.2015 |
                   |   3 | the longest string |  6 | 25.12.2015 |
                   |  44 |      longer string |  9 | 03.03.2013 |


               `(sort-dataset dataset [{:a :ascnum}]) ; sort by column :a in ascending numerical order =>`

                   |  :a |                 :b | :c |         :d |
                   |-----+--------------------+----+------------|
                   |   2 |             string |  1 | 01.01.2015 |
                   |   3 | the longest string |  6 | 25.12.2015 |
                   |  44 |      longer string |  9 | 03.03.2013 |
                   | 111 |             string |  3 | 03.11.2015 |


               `(sort-dataset dataset [{:b :asclen}]) ; sort by column :b in ascending order by field length =>`


                   |  :a |                 :b | :c |         :d |
                   |-----+--------------------+----+------------|
                   |   2 |             string |  1 | 01.01.2015 |
                   |   3 | the longest string |  6 | 25.12.2015 |
                   |  44 |      longer string |  9 | 03.03.2013 |
                   | 111 |             string |  3 | 03.11.2015 |
                   
                   
               `(sort-dataset dataset [{:d :ascdate}]) ; sort by column :d in ascending order by date =>`


                   |  :a |                 :b | :c |         :d |
                   |-----+--------------------+----+------------|
                   |  44 |      longer string |  9 | 03.03.2013 |
                   |   2 |             string |  1 | 01.01.2015 |
                   | 111 |             string |  3 | 03.11.2015 |
                   |   3 | the longest string |  6 | 25.12.2015 |


               `(sort-dataset dataset [{:b asclen} {:a :asclen}]) ; sort by column :b in ascending order by field length, for equal values arrange by column :a in ascending order by length =>`


                   |  :a |                 :b | :c |         :d |
                   |-----+--------------------+----+------------|
                   |   2 |             string |  1 | 01.01.2015 |
                   | 111 |             string |  3 | 03.11.2015 |
                   |  44 |      longer string |  9 | 03.03.2013 |
                   |   3 | the longest string |  6 | 25.12.2015 |



               `(sort-dataset dataset [{:a asclen} {:b :asclen}]) ; sort by column :a in ascending order by field length, for equal values arrange by column :b in ascending order by length =>`


                   |  :a |                 :b | :c |         :d |
                   |-----+--------------------+----+------------|
                   |   2 |             string |  1 | 01.01.2015 |
                   |   3 | the longest string |  6 | 25.12.2015 |
                   |  44 |      longer string |  9 | 03.03.2013 |
                   | 111 |             string |  3 | 03.11.2015 |
                   
                   Note: sorting by date requires dates in column to be in 'dd.mm.yyyy' format( for conversion date-literal function may be used)
                   " 
  ;[dataset colnames sorttype mode] 
   [dataset colnames-sorttypes]
  
  
      (-> (make-dataset  
            ;(sort  #(loop [ctr 0 ]
            ;                                              (if  (or (not= (f ((get colnames ctr) %1) ((get colnames ctr) %2))
            ;                                                         (f ((get colnames ctr) %2) ((get colnames ctr) %1))) (= ctr (- (count colnames) 1)))
            ;                                               (f ((get colnames ctr) %1) ((get colnames ctr) %2))
            ;                                               (recur (inc ctr) ))
            ;                                                )  (:rows dataset))

            (sort  #(loop [cs colnames-sorttypes]
                      (let [current (first cs)
                            f (get-comparator (val (first current)))
                            col (key (first current))]
                   ;     (println (str "Comparing " (str (col %1)) " -- " (str (col %2)) " = " (str (f (col %1) (col %2)))))
                                                          (if  (or (= (count cs) 1) 
                                                                   (not= 
                                                                   (f (col %1) (col %2))
                                                                   (f (col %2) (col %1))
                                                                         ) )
                                                            (f (col %1) (col %2))

                                                          
                                                           (recur (rest cs) ))
                                                            )
                  )  (:rows dataset))

                        (column-names dataset)) (with-meta (meta dataset)))
      )

(defn shift-column "Changes column's position inside a dataset. Two options are available:
  1. Takes a dataset and column name/index and moves this column to the last position, data columns with indices greater than 
  specified index will be moved one position left e.g.
          
          Given original dataset
                
                | :a | :b | :c | :d |
                |----+----+----+----|
                |  1 |  2 |  3 |  a |
                |  4 |  5 |  6 |  b |
                |  7 |  8 |  9 |  c |


          function returns the following result:
            
          `(shift-column :b) ; =>`
          
          which is equivalent to
          `(shift-column 1) ; =>`

                | :a | :c | :d | :b |
                |----+----+----+----|
                |  1 |  3 |  a |  2 |
                |  4 |  6 |  b |  5 |
                |  7 |  9 |  c |  8 |


  2. Takes a dataset, column name/index and index where this column should be moved, moves given column to the specified index.  
  Other columns will be shifted appropriately.
          
          Given original dataset
                
                | :a | :b | :c | :d |
                |----+----+----+----|
                |  1 |  2 |  3 |  a |
                |  4 |  5 |  6 |  b |
                |  7 |  8 |  9 |  c |


          function returns the following result:
            
          `(shift-column :c 0) ; =>`
          
          which is equivalent to
          `(shift-column 2 0) ; =>`
                
                
                | :c | :a | :b | :d |
                |----+----+----+----|
                |  3 |  1 |  2 |  a |
                |  6 |  4 |  5 |  b |
                |  9 |  7 |  8 |  c |

"
 ([dataset column]
     (let [data (:rows dataset)
           header (column-names dataset)
           colname (if (keyword? column) column (get (column-names dataset) column))]

        (-> (make-dataset data (conj (into [] (remove #{colname} header )) colname))
                         (with-meta (meta dataset)))))
 
 ([dataset column position-to]
     (let [data (:rows dataset)
           header (column-names dataset)
           colname (if (keyword? column) column (get (column-names dataset) column))
           position-from (.indexOf header colname)
           last-pos (- (count header) 1)]

        (-> (make-dataset data (into []   (cond 
                                            (>= position-to last-pos)
                                              (shift-column colname)
                                            
                                            (< position-from position-to) 
                                              (concat
                                                (subvec header 0 position-from)
                                                (subvec header (+ position-from 1) (+ position-to 1))
                                                [colname]
                                                (subvec header (+ position-to 1)))
                                            
                                            (>= position-from last-pos)
                                              (concat
                                                (subvec header 0 position-to)
                                                [colname]
                                                (subvec header  position-to last-pos))
                                              
                                            :else
                                              (
                                               concat
                                                (subvec header 0 position-to)
                                                [colname]
                                                (subvec header position-to position-from)
                                                (subvec header (+ position-from 1)))
                                            )
                                    )
                                            
                                      ) 
                         (with-meta (meta dataset)))))
 )
 
 
(defn remove-columns "Removes columns from a dataset. Two options are available:
  1. Takes a dataset and vector of column names and creates a new dataset containing all columns except of those that were specified.

         
            Given original dataset
                  
                     | :col1 | :col2 | :col3 | :col4 |
                     |-------+-------+-------+-------|
                     |     1 |     2 |     3 |     4 |
                     |     5 |     6 |     7 |     8 |
                     |     9 |    10 |    11 |    12 |


          function returns the following result:
            
          `(remove-columns [:col1 :col4]) ; =>`

                     | :col2 | :col3 |
                     |-------+-------|
                     |     2 |     3 |
                     |     6 |     7 |
                     |    10 |    11 |


  2. Takes a dataset and two indices and creates a new dataset containing all columns execept of columns having indices within 
     the specified interval (including both points)                     

          Given original dataset

                     | a | b | c | d | e | f |
                     |---+---+---+---+---+---|
                     | 0 | 0 | 0 | 0 | 0 | 0 |
                     | 1 | 1 | 1 | 1 | 1 | 1 |
                     | 2 | 2 | 2 | 2 | 2 | 2 |


          function returns the following result:

          `(remove-columns 2 4) ; Remove columns having indices within the interval [2, 4] =>`

                     | a | b | f |
                     |---+---+---|
                     | 0 | 0 | 0 |
                     | 1 | 1 | 1 |
                     | 2 | 2 | 2 |

          " 
  ([dataset cols] (columns dataset (remove (fn [item] (some (fn [a] (= item a)) cols)) (column-names dataset))))
  ([dataset indexFrom indexTo] (cond 
                                 (= indexTo (count (column-names dataset))) 
                                    (columns dataset (range 0 indexFrom))  
                                 :else
                                    (columns dataset (concat 
                                                       (range 0 indexFrom)
                                                       (range (+ indexTo 1) (count (column-names dataset)))))))

  )

(defn merge-columns "Merges several columns in one using specified separator between columns. Two options are available:
  1. Takes a dataset, vector of columns and separator and merges columns together. Column containing the result of the merge 
  gets the same name as the first column in the list of arguments:
                    
          Given original dataset:
                    
                    | :name |   :city | :country |            :email |
                    |-------+---------+----------+-------------------|
                    | Alice |    Oslo |   Norway | alice@example.com |
                    |   Bob | Drammen |   Norway |   bob@example.com |

   

          function returns the following result:

          `(merge-columns [:city :country] \", \") ;  =>`

                    | :name |           :city |            :email |
                    |-------+-----------------+-------------------|
                    | Alice |    Oslo, Norway | alice@example.com |
                    |   Bob | Drammen, Norway |   bob@example.com |

                    
  2. Takes a dataset, vector of columns, separator and new column name and merges columns together.

          Given original dataset:
 
                    | :name |   :city | :country |            :email |
                    |-------+---------+----------+-------------------|
                    | Alice |    Oslo |   Norway | alice@example.com |
                    |   Bob | Drammen |   Norway |   bob@example.com |

   

          function returns the following result:

          `(merge-columns [:city :country] \", \" :place) ;  =>`

                    | :name |          :place |            :email |
                    |-------+-----------------+-------------------|
                    | Alice |    Oslo, Norway | alice@example.com |
                    |   Bob | Drammen, Norway |   bob@example.com |

                    "
  ([dataset columns separator]
  (let [pos (.indexOf (column-names dataset) (nth columns 0))
        [colon & colname] (str (nth columns 0))
        tempname (keyword (str (apply str colname) "_merged_temp"))] 
    (-> (derive-column dataset tempname columns (fn [& strings] (clojure.string/join separator strings)))
        (shift-column tempname pos)
        (remove-columns columns) 
        (rename-columns {tempname (keyword (str (apply str colname)))}))
    ))
  ([dataset columns separator newname]
  (let [pos (.indexOf (column-names dataset) (nth columns 0))]
    (-> (derive-column dataset newname columns (fn [& strings] (clojure.string/join separator strings)))
        (shift-column newname pos)
        (remove-columns columns))
    ))
  )

(defn remove-duplicates "Removes duplicates from a dataset. Two options are available:
  
  1. Given a dataset sorts it and looks for rows having the same values across all columns and leaves only one instance 
  from each set of such rows, other rows(duplicates) will be removed from a dataset.
                        
        Given original dataset:
                    
                    | :name |    :age |  :gender |
                    |-------+---------+----------|
                    | Alice |      18 |   female |
                    |   Bob |      30 |     male |
                    | Alice |      28 |   female |
                    | Alice |      18 |   female |
                    |   Bob |      32 |     male |

   

          function returns the following result:

          `(remove-duplicates dataset) ;  =>`

                    | :name |    :age |  :gender |
                    |-------+---------+----------|
                    | Alice |      18 |   female |
                    | Alice |      28 |   female |
                    |   Bob |      30 |     male |
                    |   Bob |      32 |     male |
                        
  2. Given a dataset and a column(sequence of columns) looks for rows having the same values in the specified field(s) and leaves 
  only the first encountered row in this sequence. Dataset should be sorted in desired order before function is called
                    
                        
        Given original dataset:
                    
                    | :name |    :age |  :gender |
                    |-------+---------+----------|
                    | Alice |      18 |   female |
                    |   Bob |      30 |     male |
                    | Alice |      28 |   female |
                    |   Bob |      32 |     male |

   

        function returns the following result:

          `(-> (sort-dataset dataset  [:name :age] :alpha :desc) (remove-duplicates [:name :gender]) ;  Dataset is first sorted in a such way 
                                                                                              that the records about the same person 
                                                                                              are given in descending order by age =>`

                    | :name |    :age |  :gender |
                    |-------+---------+----------|
                    | Alice |      28 |   female |
                    |   Bob |      32 |     male |


                        "
  ([dataset] (-> (make-dataset (distinct (:rows dataset)) (column-names dataset)) (with-meta (meta dataset))))
  ([dataset colnames]
   (let [ ds-rows (:rows dataset) 
          grouped-rows   (for [m (group-by #(select-keys % colnames) ds-rows)] 
                         (into {} (for [groupvar (key m)]
                                   (assoc (apply merge-with (fn [& args] (first (into [] args))) 
                                                            (map #(dissoc % (key groupvar)) (val m)))                                  
                                   (key groupvar) (val groupvar)))))
]
     
             (-> (make-dataset grouped-rows (column-names dataset)) (with-meta (meta dataset)))    
     )
   )
  ;([dataset colnames colnames-separators]
   
  ; (let [ds-rows (:rows dataset)
   ;     grouped-rows   (for [m (group-by #(select-keys % colnames) ds-rows)] 
    ;                     (into {} (for [groupvar (key m)]
     ;                              (assoc (into {} (for [keyval colnames-separators] 
      ;                                        (apply merge-with (fn [& args] (clojure.string/join (val keyval) (into [] args))) 
       ;                                                         (map #(hash-map  (key keyval) (get % (key keyval))) (val m)))
        ;                                  )
         ;                                )
          ;                (key groupvar) (val groupvar))))) ]
;TODO: map all columns to string
           ;  (-> (make-dataset grouped-rows (column-names dataset)) (with-meta (meta dataset)))   
            ;)) 
   
  ;([dataset colnames separator]
   
  ; (let [ds-rows (:rows dataset)
  ;       cols-to-merge (remove (fn [i] (some #(= i %) colnames)) (column-names dataset))
  ;       grouped-rows   (for [m (group-by #(select-keys % colnames) ds-rows)] 
  ;                       (into {} (for [groupvar (key m)]
  ;                                 (assoc (into {} (for [merge-col cols-to-merge] 
  ;                                            (apply merge-with (fn [& args] (clojure.string/join separator (distinct (into [] args)))) 
  ;                                                              (map #(hash-map  merge-col (get % merge-col)) (val m)))
  ;                                        )
  ;                                       )
  ;                        (key groupvar) (val groupvar))))) ]
;TODO: map all columns to string
  ;           (-> (make-dataset grouped-rows (column-names dataset)) (with-meta (meta dataset)))   
  ;          ))
  ) 


(defn cast "Cast function is reverse to melt. Given a dataset, variable-column, value-column and name of aggregation function, it forms column headers by identifying distinct variables and populates these columns by taking  values  and performing specified aggregation on them. Other columns are treated as pivot keys.

               Given original dataset:

                    |      :company-name |             :position |  :total-employed |
                    |--------------------+-----------------------+------------------|
                    |              Cisco | Jr.Software developer |               22 |
                    |              Cisco | Sr.Software developer |               10 |
                    |              Cisco |                Intern |                2 |
                    | Oracle corporation |        Assist.manager |                2 |
                    | Oracle corporation | Sr.Software developer |               38 |
                    |                IBM |        Assist.manager |                2 |
                    |                IBM | Jr.Software developer |                8 |
                    |              Cisco |        Assist.manager |                3 |
                    |                IBM | Sr.Software developer |                5 |
                    |                IBM |                Intern |                4 |


               function returns the following result:

               `(cast :position :total-employed \"SUM\") ;  =>`


                    |      :company-name | :Jr.Software developer | :Sr.Software developer | :Intern | :Assist.manager |
                    |--------------------+------------------------+------------------------+---------+-----------------|
                    |              Cisco |                     22 |                     10 |       2 |               3 |  
                    |                IBM |                      8 |                      5 |       4 |               2 |
                    | Oracle corporation |                        |                     38 |         |               2 |

            "
  [dataset variable value f]
     (letfn [(build-aggregated-row [row] (let [rows-in-group (map #(hash-map (keyword (string-as-keyword (get % variable))) (get % value)) row)]
                                            (case f
                                              "COUNT"
                                              (frequencies (mapcat keys rows-in-group))
                                              "COUNT-DISTINCT"
                                              (frequencies (mapcat keys (distinct rows-in-group)))
                                              ("MIN" "MAX" "SUM" "AVG")
                                              (apply merge-with (ns-resolve 'tabular_functions.aggregations (symbol f)) rows-in-group)
                                              (apply merge-with (fn [& args] (clojure.string/join f (distinct (into [] args)))) rows-in-group)
                                            )))             ]
       (let [canonicalise-key (partial resolve-column-id dataset)
         input-columns (map canonicalise-key (column-names dataset))
         cols-to-raise (map canonicalise-key [variable value])
         pivot-keys (clojure.set/difference (set input-columns) (set cols-to-raise)) 
         ordered-pivot-keys (keep pivot-keys input-columns)
         casted-rows (for [m (group-by #(select-keys % pivot-keys) (:rows dataset))]  (
                                                                                     into {} (for [groupvar (key m)] (assoc (into {} (build-aggregated-row (val m))) (key groupvar) (val groupvar)))))
        new-columns (set ( into [] (->> dataset 
                                            :rows
                                            (map (fn [row]
                                                     (keyword (string-as-keyword (get row variable)))))
                                            
                                            )))
                                                        
        ]
   (-> (make-dataset casted-rows 
                     (concat ordered-pivot-keys new-columns))
       (with-meta (meta dataset)))
    )
  )
)

(defn group-rows "Given a dataset, vector of column names and set of maps of form  {colname function-or-separator-name} and creates a new 
  dataset containg rows grouped by colnames from vector and the result of applying functions to correspondent column values. If function name is not recognized as a common aggregation function, argument will be used as a separator for merged values 
  Each function in a map should take sequence of values as  a parameter and return a single value. 
               
                 For most common aggregations there exists a set of pre-defined functions:
                 - MIN
                 - MAX
                 - SUM
                 - AVG
                 - COUNT
                
        
        Example1. Given original dataset:
                   

 
                 | :firstname | :lastname | :order_num | :total_items | :total_cost |
                 |------------+-----------+------------+--------------+-------------|
                 |      Alice |     Smith |       1111 |            5 |         150 |
                 |        Bob |   Johnson |        857 |            7 |          70 |
                 |      Alice |     Smith |       1112 |           30 |         340 |
                 |      Alice |  Williams |        505 |            1 |         170 |
                 |        Bob |   Johnson |        858 |            3 |         370 |
                 |       Mary |  Williams |       1543 |            1 |          15 |

   

          function returns the following result:

          `(group-rows dataset [:firstname :lastname] #{ {:total_items \"SUM\"}   ; total number of items person ordered
                                                    #_=> {:total_cost \"AVG\"}    ; average total cost per one order
                                                    #_=> {:order_num \"COUNT\"}   ; number of orders person made
                                                    #_=> {:total_cost \"MAX\"}})  ; maximum total_cost per one order =>`

                 | :firstname | :lastname | :order_num_COUNT | :total_cost_AVG | :total_items_SUM | :total_cost_MAX |
                 |------------+-----------+------------------+-----------------+------------------+-----------------|
                 |      Alice |     Smith |                2 |             245 |               35 |             340 |
                 |        Bob |   Johnson |                2 |             220 |               10 |             370 |
                 |      Alice |  Williams |                1 |             170 |                1 |             170 |
                 |       Mary |  Williams |                1 |              15 |                1 |              15 |

                    
                        
        Example2. Given original dataset:
                    
                    | :name |    :phone-number |  
                    |-------+------------------|
                    | Alice |        123-45-67 |
                    |   Bob |        777-88-99 |
                    | Alice |        111-11-11 |

   

        function returns the following result:

          `(group-rows dataset [:name] #{{:phone-nuber \", \"}}) ;  =>`

                    | :name |        :phone-number |  
                    |-------+----------------------|
                    | Alice | 123-45-67, 111-11-11 |
                    |   Bob |            777-88-99 |
                 "
  [dataset colnames colnames-functions ]
  (letfn [(build-aggregated-row [row colname function] 
    (let [new-colname (keyword (str (name colname) "_" (case function 
                                                         ("COUNT" "COUNT-DISTINCT" "MIN" "MAX" "SUM" "AVG") function
                                                          "MERGED")))
          rows-in-group (map #(hash-map new-colname (get % colname)) row)]
      (case function 
        "COUNT" (frequencies (mapcat keys rows-in-group))
        "COUNT-DISTINCT" (frequencies (mapcat keys (distinct rows-in-group)))
        ("MIN" "MAX" "SUM" "AVG")  (apply merge-with (ns-resolve 'tabular_functions.aggregations (symbol function)) rows-in-group)
        (apply merge-with (fn [& args] (clojure.string/join function (distinct (into [] args)))) rows-in-group)  )))] 
   (let [ds-rows (:rows dataset)
         grouped-rows   (for [m (group-by #(select-keys % colnames) ds-rows)] 
                         (into {} (for [groupvar (key m)]
                                   (assoc 
                                     (into {} (for [keyval colnames-functions] 
                                             (build-aggregated-row (val m) (key (first keyval)) (val (first keyval)))))
                                          (key groupvar) (val groupvar)
                                          )
                                   )))
        new-colnames (concat colnames (for [keyval colnames-functions] (keyword (str (name (key (first keyval))) "_" (case (val (first keyval))
                                                ("MIN" "MAX" "SUM" "COUNT" "COUNT-DISTINCT" "AVG")  (val (first keyval))
                                                "MERGED")
                                                                                                                       ))))
        ]
        (-> (make-dataset grouped-rows new-colnames)(with-meta (meta dataset)))
     )
) )

(defn merge-rows [dataset from to]
  (letfn [(build-merged-row [rows]
              (apply merge-with (fn [& args] (clojure.string/join " " (distinct (into [] args)))) rows)
            )]
    (let [ds-rows (-> dataset  (rows (range from to))(:rows) (into []))
          colnames (column-names dataset)
          eods (count (:rows dataset))
          preceding-rows  (-> dataset (rows (range 0  from)) (:rows))
          subsequent-rows (-> dataset (rows (range to  eods)) (:rows))
          merged-row (list (into {} (build-merged-row ds-rows))) 
                          
          ]
      (-> (make-dataset 
            (concat preceding-rows merged-row subsequent-rows) 
            colnames)
                    (with-meta (meta dataset)))
  )))
(defn join-dataset "Joins two datasets together. Two options are available:
                   
  1. Takes a dataset, filename and type of concatenation (either :v to concatenate datasets vertically -- append data from file 
  to the right side of given dataset or :h to concatenate datasets horizontally -- append data from file to the bottom of 
  given dataset). 
  
  Throws an error if number of columns/rows is not appropriate
                   
  Examples.

        Given original dataset:
                    
                    | :name |    :age |  :gender |
                    |-------+---------+----------|
                    | Alice |      18 |   female |
                    |   Bob |      30 |     male |
        
        file 'left-part.csv' with content:
                   
                   email,country
                   alice@example.com,Norway
                   bob@example.com,Norway
        

          function returns the following result:

          `(join-dataset \"left-part.csv\" :v) ;  =>`

                    | :name |    :age |  :gender |            :email | country | 
                    |-------+---------+----------+-------------------+---------|
                    | Alice |      18 |   female | alice@example.com |  Norway |
                    |   Bob |      30 |     male |   bob@example.com |  Norway |

       
        Given the same original dataset and file 'other-persons.csv' with content:
                   
                   name,age,gender
                   John,38,male
                   Mary,27,female

          function returns the following result:

          `(join-dataset \"other-persons.csv\" :h) ;  =>`
                    
                    | :name |    :age |  :gender |
                    |-------+---------+----------|
                    | Alice |      18 |   female |
                    |   Bob |      30 |     male |
                    |  John |      38 |     male |
                    |  Mary |      27 |   female |
        
              
  2. Takes a dataset, filename, column that acts as foreign key in original dataset and columns for id and value in file. 
  Builds a lookup table from file and maps values in original dataset appropriately.
                   
        Given original dataset:
                    
                    | :name |    :age |  :position |
                    |-------+---------+------------|
                    | Alice |      18 |         25 |
                    |   Bob |      30 |          7 |
        
        file 'position-codes.csv' with content:
                   
                   code,description
                   1,manager
                   7,engineer
                   8,senior engineer
                   25,accountant
        

          function returns the following result:

          `(join-dataset \"position-codes.csv\" :position \"code\" \"description\") ;  =>`


                    | :name |    :age |  :position |
                    |-------+---------+------------|
                    | Alice |      18 | accountant |
                    |   Bob |      30 |   engineer |
                   "
  ([dataset filename concat-type]
  (cond (= concat-type :h) (-> (make-dataset (:rows (incanter.core/conj-rows dataset (map reverse(->(read-dataset filename)(drop-rows 1)(:rows))) )) (column-names dataset))(with-meta (meta dataset)))
        (= concat-type :v)
        dataset
    )

   )
  ([dataset filename fkey id value]
   (mapc dataset { fkey #((zipmap (map second (map first (-> (read-dataset filename) (-> (make-dataset move-first-row-to-header) (rename-columns (comp keyword string-as-keyword))) (columns [(keyword id)]) (:rows)))) (map second (map first (-> (read-dataset filename) (-> (make-dataset move-first-row-to-header) (rename-columns (comp keyword string-as-keyword))) (columns [(keyword value)]) (:rows))))) %)})
   )
  )

(defn split-column "Given a dataset, column name and separator splits specified column into multiple by separator. New columns get 
 names of a form [original-column-name]_splitted_0, [original-column-name]_splitted_1, ...

                   
          Given original dataset:

                    | :name |                       :address |            :email |
                    |-------+--------------------------------+-------------------|
                    | Alice | New York, Harrison Street, 507 | alice@example.com |
                    |   Bob |      Richmond, Main Street, 17 |   bob@example.com |
                    |  Mary | NY, Harrison Street, 29, H0512 |  mary@example.com |


   

          function returns the following result:

          `(split-column :address #\", \") ;  =>`


                   | :name | :address_splitted_0 | :address_splitted_1 | :address_splitted_2 | :address_splitted_3 |            :email |
                   |-------+---------------------+---------------------+---------------------+---------------------+-------------------|
                   | Alice |            New York |     Harrison Street |                 507 |                     | alice@example.com |
                   |   Bob |            Richmond |         Main Street |                  17 |                     |   bob@example.com |
                   |  Mary |                  NY |     Harrison Street |                  29 |               H0512 |  mary@example.com |

 " 
  [dataset colname separator]
  ;( derive-column dataset :new [colname] (fn [col] (clojure.string/split col separator)))


      (let [ col-pos (.indexOf (column-names dataset) colname)
            [colon & columnname] (str colname)
            new-rows   (->> dataset
                            :rows
                            (map (fn [row]
                                  (let [value-in-row (get row colname)
                                  new-col-vals (clojure.string/split value-in-row separator)
                                  index-last (- (count new-col-vals) 1)]
                                          
                                      
                                  (loop [i 0 rowmap row]
                                       (if (> i index-last)
                                         rowmap
                                         (recur (inc i)
                                                (assoc rowmap (keyword (str (apply str columnname) "_splitted_" (str i)))  (get new-col-vals i)))
                                       )
                                  )
                                  ))
                              ))
            new-columns   (set (apply concat (->> dataset
                            :rows
                            (map (fn [row]
                                  (let [value-in-row (get row colname)
                                  new-col-vals (clojure.string/split value-in-row separator)
                                  index-last (- (count new-col-vals) 1)]
                                          
                                      
                                  (loop [i 0 newcols #{}]
                                       (if (> i index-last)
                                          newcols
                                         (recur (inc i)(conj newcols (keyword (str (apply str columnname) "_splitted_" (str i))) ))
                                                
                                       )
                                  )
                                  ))
                              ))))

            ]
      ;new-rows
            ( -> (make-dataset new-rows 
                               (concat (subvec (column-names dataset) 0 col-pos) (sort new-columns)(subvec (column-names dataset) (+ col-pos 1))))
                 (with-meta (meta dataset)))
      ))

