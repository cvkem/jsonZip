(ns vinzi.jsonZip.test
  (:use [vinzi.jsonZip] :reload)
  (:use [clojure [walk :only [postwalk]]
	 pprint
	 test])
  (:require  [clojure.contrib
	      [json :as json]
	      [duck-streams :as ds :only [reader]]])
  (:require [clojure.zip :as zip]))

(println)
(println "You should consider to run the test-suite of package vinzi.jsonDiff for an additional set of testcases!!!")

;;(def *stack-trace-depth* 10)

;(comment 

;; helper routines to bring structure to canonical-form

(defn- strCompare [x y]
  (compare (str x) (str y)))

(defn- sort-map [m]
  (into (sorted-map-by strCompare) m))

(defn- sort-set [s]
  (into (sorted-set-by strCompare) s))

(defn- sForm [f]
  (if (map? f)
    (sort-map f)
    (if (set? f)
      (sort-set f)
      f)))

(defn sortForm [f]
  (postwalk sForm f))

(defn equalForms [x y]
  (let [sx  (sortForm x)
	sy  (sortForm y)
	res (strCompare sx sy)]
    (when (not= res 0)
      (println  "equalForms:   " res)
      (print "with x: ") (pprint sx)
      (print "with y: ") (pprint sy))
    (= res 0)))


;; end helper routines

;;
;; The test data
;;

(def testTree {:level_0	{:level_0_1  "de titel"
			 :level_0_2   []
			 :level_0_3  ["text"]}
	       :level_1  {:level_1_1 3
		       :level_1_2 5}
	       :level_2  "de file"})

(def modTestTree (jsonToZippertree testTree))

(def testZip (jsonZipper testTree))

(def backJson (zippertreeToJson modTestTree))


(def res_1 {:level_0	{:level_0_1  "de titel"
			 :level_0_2   []
			 :level_0_3  ["text"]}
	    :level_1  {:level_1_1 3
		       :level_1_2 5}
	   :level_2  "de file"
           :test {"TEST" "inserted"}})


(def res_2 {:level_0	{:level_0_1  "de titel"
			 :level_0_2   []
			 :level_0_3  ["text"   {"TEST" "inserted"} ]}
	    :level_1  {:level_1_1 3
		       :level_1_2 5}
	   :level_2  "de file"})

(def res_3 {:level_0	{:level_0_1  "de titel"
			 :level_0_2   []
			 :level_0_3  [{"TEST" "inserted"} "text"   ]}
	    :level_1  {:level_1_1 3
		       :level_1_2 5}
	   :level_2  "de file"})

(def insElem2   (setJsonKey :test 99))

(def res_4 {:level_0	{:level_0_1  "de titel"
			 :level_0_2   []
			 :level_0_3  ["text"]}
	    :level_1  {:level_1_1 3
		       :level_1_2 5}
	   :level_2  "de file"
           :test (first insElem2) })   ;; need to unbox it here


(def insElement (setJsonKey :test {"TEST" "inserted"}))


(deftest testSuite 
  (is (equalForms testTree  (zippertreeToJson (jsonToZippertree testTree))))  ;; transform json and transform back
  (is (equalForms testTree (zippertreeToJson (zip/node testZip))))   ;; insert tree in zipper and extract it (undamaged)
  (is (equalForms testTree (zippertreeToJson (zip/root testZip))))   ;; insert tree in zipper and extract it (undamaged)
  (is (equalForms res_1 (zippertreeToJson (zip/root (zip/append-child testZip insElement)))))
  (is (equalForms res_1 (zippertreeToJson (zip/root (zip/insert-child testZip insElement)))))
  (is (equalForms res_1 (jsonRoot (zip/insert-child testZip insElement))))
  (is (equalForms res_2 (zippertreeToJson (zip/root (zip/append-child (zip/right (zip/down (zip/down testZip))) insElement)))))
  (is (equalForms res_3 (->> insElement
			    (zip/insert-child (-> testZip (zip/down) (zip/down) (zip/right)))
			    (zip/root)
			    (zippertreeToJson))))
  (is (equalForms res_4 (zippertreeToJson (zip/root (zip/append-child testZip insElem2)))))
  (is (equalForms "/"  (jsonPathStr testZip)))
  (is (equalForms "/level_0"  (jsonPathStr (zip/down testZip))))
  (is (equalForms "/level_1"  (jsonPathStr (zip/right (zip/down testZip)))))
  (is (equalForms '("/" "level_1")  (jsonPathList (zip/right (zip/down testZip)))))
      )

;; test dataset 2
(def testTree2 [1
	       {:level_0a  1
		:level_0b []}
	       :b])
(def modTestTree2 (jsonToZippertree testTree2))
(def backJson2 (zippertreeToJson modTestTree2))
(def testZip2 (jsonZipper testTree2))

(def res2_1 [1
	       {:level_0a  1
		:level_0b []}
	     :b
	     {"TEST" "inserted"}])
(def res2_2 [{"TEST" "inserted"}
	       1
	       {:level_0a  1
		:level_0b []}
	     :b
	     ])
(def res2_3 [1
	       {:level_0a  1
		:level_0b []
		:test {"TEST" "inserted"}}
	       :b])
(def res2_4 [1
	       {:level_0a  1
		:level_0b [{"TEST" "inserted"}]}
	       :b])


(deftest testSuite2 
  (is (equalForms testTree2  (zippertreeToJson (jsonToZippertree testTree2))))  ;; transform json and transform back
  (is (equalForms testTree2 (zippertreeToJson (zip/node testZip2))))   ;; insert tree in zipper and extract it (undamaged)
  (is (equalForms testTree2 (zippertreeToJson (zip/root testZip2))))   ;; insert tree in zipper and extract it (undamaged)
  (is (equalForms res2_1 (zippertreeToJson (zip/root (zip/append-child testZip2 insElement)))))
  (is (equalForms res2_2 (zippertreeToJson (zip/root (zip/insert-child testZip2 insElement)))))
  (is (equalForms res2_3 (zippertreeToJson (zip/root (zip/append-child (zip/right (zip/down testZip2)) insElement)))))
   (is (equalForms res2_4 (->> insElement
   			    (zip/insert-child (-> testZip2 (zip/down) (zip/right) (zip/down)))
   			    (zip/root)
   			    (zippertreeToJson))))
  (is (equalForms "/"  (jsonPathStr testZip2)))
  (is (equalForms "/[0]"  (jsonPathStr (zip/down testZip2))))
  (is (equalForms "/[1]"  (jsonPathStr (zip/right (zip/down testZip2)))))
  (is (equalForms '("/" "[1]")  (jsonPathList (zip/right (zip/down testZip2)))))
      )


(def largeJson (with-open [data (ds/reader "./data/EIS-poc.cdfde")]
		 (json/read-json data)))

;;;;;;;;;;;;
;;   test 3   transform a large json file to a zipper and translate it it back.
;;

(deftest testSuite3
  (is (equalForms largeJson  (zippertreeToJson (jsonToZippertree largeJson))))  ;; transform json and transform back
  )

;;;;;;;;;;;;;;;
;; test 4   Key handling for vectors (using the "id" field or not)
;;      (test 8 tests using "name" as vector-id)
;;

(def test4a (jsonZipper [{"NO-id" 1  :b "mod-string"}
			 {"test" 2  :b  3}
			 ]))
(def key4a "[0]")

(def test4b (jsonZipper [{"id" 1  :b "mod-string"}
			 {"test" 2  :b  3}
			 ]))
(def key4b "1")

(def test4c (jsonZipper [{"id" 1  :b "mod-string"}
			 {"id" 2  :b  3}
			 ]))
(def key4c "1")


(def key4de "ABCD")
(def test4d (jsonZipper [{"id" key4de  :b "mod-string"}
			 {"id" 2  :b  3}
			 ]))

(def key4e "XYZ")
(def test4e (jsonZipper {:a [{"id" key4de  :b "mod-string"}
			     {"id" 2  :b  3}
			     ]
			 :z "test"}))


(deftest testSuite4
  (is  (= (jsonKey (zip/down test4a)) key4a))
  (is  (= (jsonKey (zip/down test4b)) key4b))
  (is  (= (jsonKey (zip/down test4c)) key4c))
  (is  (= (jsonKey (zip/down test4d)) key4de))
  (is  (= (jsonKey (zip/down (zip/down test4e))) key4de))
  )


;;;;;;;;;;;;;;;;;;
;;  test 5   insert, delete and change
(def test5a (jsonZipper {:a 1
			 :b 2
			 }))

(def test5b (jsonZipper ['(1)])) 

(deftest testSuite5
  (is (equalForms (jsonRoot (insertItem  test5a ["/"] :c "string")) {:a 1 :b 2 :c "string"})
      "Insert a string as kv-pair")
  (is (equalForms (jsonRoot (insertItem  test5a ["/"] :c 1)) {:a 1 :b 2 :c 1})
      "Insert a key-value (integer) in a map")
  (is (equalForms (jsonRoot (insertItem  test5a ["/"] :c [\a \b \c])) {:a 1 :b 2 :c [\a \b \c]})
      "Insert a vector in a map")
  (is (equalForms (jsonRoot (removeItem  test5a ["/"] :a)) {:b 2})
      "remove a single item from a map")
  (is (equalForms (jsonRoot (removeItem (removeItem  test5a ["/"] :a)
					["/"] :b)) {})
      "removing all items of a map returns an empty map")
  (is (equalForms (jsonRoot (replaceItem  test5a ["/"] :b 3)) {:a 1 :b 3})
      "remove a single item from a map")
  (is (equalForms (jsonRoot (replaceItem  test5a ["/"] :b [\a \b \c])) {:a 1 :b [\a \b \c]})
      "remove a single item from a map")
  (is (equalForms (jsonRoot (replaceItem  test5a ["/"] :a -2)) {:a -2 :b 2})
      "remove a single item from a map")
  (is (-> test5a
	  (removeItem ["/"] :a)
	  (removeItem ["/"] :b)
	  (jsonRoot)
	  (equalForms {}))
      "removing alle items of a map.")
  (is (-> test5b
	  (removeItem ["/"] "[0]")
	  (jsonRoot)
	  (equalForms []))
      "removing all items from a vector")
  (is (equalForms (jsonRoot (insertItem  test5b ["/"] [1] "string")) ['(1) "string"])
      "Insert a string in a vector")
  (is (equalForms (jsonRoot (insertItem  test5b ["/"] [1] -2)) ['(1) -2])
      "Insert a map in a vector")
  (is (equalForms (jsonRoot (insertItem  test5b ["/"] [1] [\a \b \c])) ['(1) [\a \b \c]])
      "Insert a vector in a map")
  )



;;;;;;;;;;;;;;;;;;
;;  test 6: repeat tests one level deeper (insert, delete and change)

(def test6a (jsonZipper [0
			 {:a 1
			 :b 2
			 }]))

(def test6b (jsonZipper ['(1)])) 

(deftest testSuite6
  (is (equalForms (jsonRoot (insertItem  test6a ["/" "[1]"] :c "string")) [0 {:a 1 :b 2 :c "string"}])
      "Insert a string as kv-pair")
  (is (equalForms (jsonRoot (insertItem  test6a ["/" "[1]"] :c 1)) [0 {:a 1 :b 2 :c 1}])
      "Insert a key-value (integer) in a map")
  (is (equalForms (jsonRoot (insertItem  test6a ["/" "[1]"] :c [\a \b \c])) [0 {:a 1 :b 2 :c [\a \b \c]}])
      "Insert a vector in a map")
  (is (equalForms (jsonRoot (removeItem  test6a ["/" "[1]"] :a)) [0 {:b 2}])
      "remove a single item from a map")
  (is (equalForms (jsonRoot (removeItem (removeItem  test6a ["/" "[1]"] :a)
  					["/" "[1]"] :b)) [0 {}])
      "removing all items of a map returns an empty map")
  (is (equalForms (jsonRoot (replaceItem  test6a ["/" "[1]"] :b 3)) [0 {:a 1 :b 3}])
      "remove a single item from a map")
  (is (equalForms (jsonRoot (replaceItem  test6a ["/" "[1]"] :b [\a \b \c])) [0 {:a 1 :b [\a \b \c]}])
      "remove a single item from a map")
  (is (equalForms (jsonRoot (replaceItem  test6a ["/" "[1]"] :a -2)) [0 {:a -2 :b 2}])
      "remove a single item from a map")
  (is (-> test6a
  	  (removeItem ["/" "[1]"] :a)
  	  (removeItem ["/" "[1]"] :b)
  	  (jsonRoot)
  	  (equalForms [0 {}]))
      "removing alle items of a map.")
  )



(def org7data [ -10
	       -11
	       -12
	       -13
	       -14])

(def org7zip (jsonZipper org7data))

(deftest testSuite7
  (is (equalForms (insertItem  org7zip ["/"] "[1]" "string")  nil)
      "Insert in middle of array not possible with index keys.")
  (is (equalForms (jsonRoot (replaceItem  org7zip ["/"] "[1]" "string"))  [-10 "string" -12 -13 -14])
      "Insert in middle of array not possible with index keys.")
  (is (-> org7zip
  	  (removeItem ["/"] "[1]")
  	  (jsonRoot)
  	  (equalForms [-10 -12 -13 -14]))
      "removing second item of array.")
  (is (-> org7zip
  	  (removeItem ["/"] "[1]")
  	  (removeItem ["/"] "[1]")
  	  (jsonRoot)
  	  (equalForms [-10 -13 -14]))
      "removing second and third item of array.")
  (is (-> org7zip
  	  (removeItem ["/"] "[1]")
  	  (removeItem ["/"] "[2]")
  	  (jsonRoot)
  	  (equalForms [-10 -13 -14]))
      "removing second and third item of array.")
  )


;;;;;;;;;;;;;;;
;; test 8   Key handling for vectors (using the "name" field or not)
;;      (test 4 containts tests usage of "id" as vector-id)
;;

(def test8a (jsonZipper [{"NO-name" 1  :b "mod-string"}
			 {"test" 2  :b  3}
			 ]))
(def key8a "[0]")

(def test8b (jsonZipper [{"name" 1  :b "mod-string"}
			 {"test" 2  :b  3}
			 ]))
(def key8b "1")

(def test8c (jsonZipper [{"name" 1  :b "mod-string"}
			 {"name" 2  :b  3}
			 ]))
(def key8c "1")


(def key8de "ABCD")
(def test8d (jsonZipper [{"name" key8de  :b "mod-string"}
			 {"name" 2  :b  3}
			 ]))

(def key8e "XYZ")
(def test8e (jsonZipper {:a [{"name" key8de  :b "mod-string"}
			     {"name" 2  :b  3}
			     ]
			 :z "test"}))


(deftest testSuite8
  (is  (= (jsonKey (zip/down test8a)) key8a))
  (is  (= (jsonKey (zip/down test8b)) key8b))
  (is  (= (jsonKey (zip/down test8c)) key8c))
  (is  (= (jsonKey (zip/down test8d)) key8de))
  (is  (= (jsonKey (zip/down (zip/down test8e))) key8de))
  )
