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
	sy  (sortForm y)]
    ;; (println  "equalForms")
    ;; (print "with x: ") (pprint sx)
    ;; (print "with y: ") (pprint sy)
    (= 0 (strCompare sx sy))))

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



(deftest testSuite3
  (is (equalForms largeJson  (zippertreeToJson (jsonToZippertree largeJson))))  ;; transform json and transform back
  )