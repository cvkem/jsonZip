(ns vinzi.jsonZip
  (:require (clojure [zip :as zip] [walk :as walk]))
  (:require clojure.pprint)
  )
;  (:gen-class))

;;  (:use [vinzi.drepl :as dr])


(def jsonTypeMap ::jsonMap)
(def jsonTypeVector ::jsonVector)


(defn jsonToZippertree
  "Transform a (json) nested map to a tree that can be editted via the Zipper
toolset. The nested map is not directly edittable as you can not insert
children via the Edit/Replace functionality.
The keys and path-strings are stored in the metadata as :json/key  and :json/path.
NOTE: The path :json/path differs from the path maintained by the zipper as the tree-structure is modified."
   ([node]
;;      (println "jsonZipper with one argument. Assuming currPath @ root ")
      (jsonToZippertree node {:json/key "/"}))
   ([node metadata]
;;     (println "jsonZipper with currPath " (:json/path metadata))
      (let [isCompound (fn [kv]
			 (let [v (second kv)]
			   (and (coll? v)
				(not (:json/boxed (meta v))))))
	    visitMap (fn [node metadata]
		       ;;		   (println "Visit Map: " node)
		       (let [k (keys node)
			     v (vals node)
			     childMeta (map #(into {:json/key %} (meta node)) k)
			     v (map #(jsonToZippertree %1 %2) v childMeta)
			     kv (partition 2 (interleave k v))
			     gbkv (group-by isCompound kv)
			     kvc (when-let [elem (find gbkv true)] (val elem))
			     kvb (when-let [elem (find gbkv false)] (val elem))
			     ;kvb (val (find gbkv false))
			     _  (assert (< (count gbkv) 3))  ;; (only values true and false present)
			     vc (map second kvc)    ;; flatten compound keys to a sequence (discard keys)
			     jsonChild (if (> (count vc) 0)          
					 (list :jsonChildren vc)     ;; ?? reverse removed ??
					 '())
			     nKvs (concat (flatten kvb) jsonChild)   ;; new key-value pairs
			     newNode  (apply array-map nKvs)         ;; array-map to keep tings in order
			     theMetadata (into metadata {:json/type jsonTypeMap})]
			 ;;			 (with-meta (apply hash-map nKvs) metadata)
			   (with-meta newNode theMetadata)
			 ))
	    visitVector (fn [node metadata]
			  ;;		      (println "Visit vector: " node)
			  (let [metas   (map #(into metadata {:json/key (str "[" % "]")} )
					     (range (count node))) ;add key (index) to metadata
;;				_ (do (println " node= " node "  and metas= " metas) (flush))   ;; DEBUG
				jsonChildren (vec (map jsonToZippertree node metas))   ;; change LIST to VECTOR
				jsonChildrenMeta (with-meta jsonChildren metadata)
				theMetadata (into metadata {:json/type jsonTypeVector})
				result  (with-meta {:jsonChildren jsonChildrenMeta} theMetadata)]
;			    (dr/debug-repl)
			    result
			    ))
	    boxWithMeta (fn [node metadata]
			  (if (contains? (parents (class node)) clojure.lang.IObj)
			    (with-meta (into metadata (meta node)))
			    (with-meta (list node) (into {:json/boxed true} metadata)))) ]
	(if (map? node)
	  (visitMap node metadata)
	  (if (vector? node)
	    (visitVector node metadata)
	    (boxWithMeta node metadata))))))

(defn zippertreeToJson
  "Translate a vinzi.jsonzipperTree back to a hash-map (json structure)"
  [node]
  (let [processMap (fn [node]
		   (let [jsonChild (:jsonChildren node)
			 ;; extract meta-data before DFS recursion!!
			 jsonChildMeta (map meta jsonChild)
			 jsonChildKey  (map :json/key jsonChildMeta)
			 jsonChild (map zippertreeToJson jsonChild)
			 node (dissoc node :jsonChildren)
			 k (keys node)
			 v (vals node)
			 ;; TO DO: check whether this code is correct (vectors also store keys!!)
			 ;isMap  (or (not-every? nil? jsonChildMeta)
					;	    (not (empty? k)))]
			 ;; using the meta-data to verify type
			 isMap  (= (:json/type (meta node)) jsonTypeMap)]
		     (if isMap
		       (let [kk (concat k jsonChildKey)
			     vv (concat v jsonChild)
	      		     newKeyVals (interleave kk vv)]
			 (apply hash-map newKeyVals))
		       (do
			 (vec jsonChild)))))]
    (if (map? node)
      (processMap node)
      (if (and (:json/boxed (meta node))
	       (list? node)
	       (= 1 (count node)))
	(first node)
	node))))  

;;
;; routines needed to generate the 'Huet' zipper for the (modified) json structures.
;;

(defn jsonBranch?
  "Test whether this node can contain children (:jsonChildren)"
  [node]
  (and (map? node)
       (not (nil? (:jsonChildren node)))))

(defn jsonChildren [node]
  (if (map? node)
    (:jsonChildren node)))

(defn jsonMakeNode [node children]
  ;; (println)
  ;; (println "Make a Node for node:")
  ;; (pprint node)
  ;; (println " with children:")
  ;; (pprint children)
  ;; (println)
  (assoc node :jsonChildren (with-meta children (meta node))))


(defn jsonZipper
  "create a jsonZipper that can be used with the standard clojure.zip tools"
  [root]
  (zip/zipper jsonBranch?
	  jsonChildren
	  jsonMakeNode
	  (jsonToZippertree root)))



;;
;; helper routines for pretty-printing data, extracting  meta-data, extracting path etc...
;;

(defn pprintJsonZipper
  "Traverses a jsonZipper structure and pretty-prints the data in the tree and the corresponding meta-data. (Meta-data is used to store the keys of the original Json-structure."
  [zipper]
  (let [children (:jsonChildren zipper)
	metaData (meta zipper)
	base     (dissoc zipper :jsonChildren)]
    (print "BASE: ")
    (clojure.pprint/pprint base)
    (print "WITH META: ")
    (clojure.pprint/pprint metaData)
    (println "NOW processing the " (count children) " children")
    (flush)
    (doseq [child children]
      (if (map? child)
	(do
	  (pprintJsonZipper child)
;	  (println " returning to BASE: " base " with META: " metaData) (flush)
	(do
	  (print "LEAF: ")
	  (clojure.pprint/pprint child)
	  (flush)))))))

(defn nodeContentsHtml
  "Show the contents of the current node as a html-string."
  [zipper]
  (let [node (zip/node zipper)
	sk    (sort (filter #(not (= :jsonChildren %)) (keys node)))
	sv    (map node sk)
	skv   (apply str (interpose "<br/>" (map #(format "<font color=blue>%s</font>:\t %s" (name %1) %2) sk sv)))
	html  (format "<html>%s</html>" skv)]
	html))

(defn nodeChildrenHtml
  "Show the contents of the current node as a html-string."
  [zipper]
  (let [node (zip/node zipper)
	chdr (:jsonChildren node)
;;	_ (do (println "chdr = " chdr) (flush)) 
	chk  (map #(:json/key (meta %)) chdr)
	cht  (map #(let [jt (:json/type (meta %))
			 jt (if (nil? jt) (type %) jt)] jt) chdr)
;;	_ (do (println "chk = " chk "  and cht = " cht) (flush)) 
	list_kv (interpose "<br/>" (map #(format "<font color=blue>%s</font>:\t %s" (name %1) %2) chk cht))]
    (if (seq list_kv)
      (do
;;	(println "list_kv= " list_kv) (flush)
	(format "<html>%s</html>" (apply str list_kv)))
      "")))   ;; If no children return "" 


(defn jsonKey
  "Return the key of the current node in the jason-zipperTree"
  [zipper]
  (:json/key (meta (zip/node zipper))))


(defn setJsonKey
  "Set a :json/key and :json/type for an element while preserving the existing metadata.
This function is only used for compound elements (collections) that will be inserted in a hashmap."
  [key element]
  (let [isIObj    (fn [o]
		    (contains? (parents (class o)) clojure.lang.IObj))
	element   (if (isIObj element)
		    element
		    (with-meta (list element) {:json/boxed true}))
	keyMeta   {:json/key key}
	typeMeta  (cond
		   (map? element) {:json/type jsonTypeMap}
		   (vector? element) {:json/type jsonTypeVector}
		   (list? element) {}    ;; boxed-element does not have :json/type 
		   :else (println "ERROR (setJsonKey only supports map or vector) passed: (" element ")"))
	metaData  (conj keyMeta typeMeta (meta element))] 
  (with-meta element metaData)))


(defn jsonType
  "Return the type of the current node in the jason-zipperTree  (jsonTypeMap or jsonTypeVector)."
  [zipper]
  (:json/type (meta (zip/node zipper))))

(defn jsonStatusNode
  "Return the key of the current node in the jason-zipperTree"
  [zipper]
  (:json/status (meta (zip/node zipper))))

(defn setJsonStatus
  "Set a :json/key and :json/type for an element while preserving the existing metadata.
This function is only used for compound elements (collections) that will be inserted in a hashmap."
  [key element]
  (let [statMeta   {:json/status key}
	metaData  (conj statMeta (meta element))]
    (assert false) ;;  "Check code of this function. Apply to element or to a node?"
    (with-meta element metaData)))




(defn jsonPathList
  "Find the JsonPathList of a sting by moving up and collecting the keys. First item will be '/' (root)."
  ([node] (jsonPathList node '() ))
  ([node cumm]
     (if (seq  node)
       (recur (zip/up node) (conj cumm (name (:json/key (meta (zip/node node))))))
       cumm)))

(defn jsonPathStr
  "Generate a '/' delimited path to the current node. Data can be a json-zipper-tree or a path-list."
  [data]
;  (dr/debug-repl)
  (if (list? data) 
    (if (seq data)
      (let [sData (map #(if (= (type %) (class 1)) (str "[" % "]") %) data) ;; map integer-keys to [i]
	  res   (apply str (cons (first sData) (interpose "/" (rest sData))))] ;; treat '/' separate
	res)
      "NONE")
    (jsonPathStr (jsonPathList data))))  ;; data is a node. Extract it's pathList



;;;;;  Tijdelijk
(comment 
;;(require 'clojure.zip :as 'zip)
;;(use 'clojure.walk)
;;(use 'clojure.pprint)


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
  (walk/postwalk sForm f))

(defn equalForms [x y]
  (let [sx  (sortForm x)
	sy  (sortForm x)]
    (println  "equalForms")
    (print "with x: ") (clojure.pprint/pprint sx)
    (print "with y: ") (clojure.pprint/pprint sy)
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


(def res2 {:level_0	{:level_0_1  "de titel"
			 :level_0_2   []
			 :level_0_3  ["text"]}
	       :level_1  {:level_1_1 3
		       :level_1_2 5}
	   :level_2  "de file"
           "TEST"      "inserted"})



)

;;;;;;;;;;;;; END tijdelijk











;;
;;Test data and test routines
;;(Should move to a the leiningen-test namespace)
;; Initially I did not use automated testing as visual inspection is needed for the tests below
;; (the Zipper-datastructure in which the test-results are embedded complicates things a bit.)
;;

(comment  ;;; stuff to test it

(def testTree {:level_0	{:level_0_1  "de titel"
			 :level_0_2   []
			 :level_0_3  ["text"]}
	       :level_1  {:level_1_1 3
		       :level_1_2 5}
	       :level_2  "de file"})

(def modTestTree (jsonToZippertree testTree))

(def testZip (jsonZipper testTree))





(def backJson (zippertreeToJson modTestTree))

;; (def testZip (zip/zipper
;; 	   jsonBranch?
;; 	   jsonChildren
;; 	   jsonMakeNode
;; 	   (jsoToZipper testTree)))



;; only used for testing
(use 'com.georgejahad.difform)


(defn testdiff []
  (difform testTree  (zippertreeToJson (jsonToZippertree testTree))))

(defn test0 []
  (println "show root")
  (clojure.pprint/pprint (zip/node testZip)))


(defn test1 []
  (println "add node at root")
  (clojure.pprint/pprint (zip/node (zip/append-child testZip {"TEST" "inserted"}))))

(defn test2 []
  (println "add node at 1 down and go to up again")
  (clojure.pprint/pprint (zip/node (zip/up (zip/append-child (zip/down testZip) {"TEST" "inserted"})))))


(defn test3 []
  (println "add node at 1 down and go to root")
  (clojure.pprint/pprint (zip/root (zip/append-child (zip/right (zip/down testZip)) {"TEST" "inserted"}))))

					;)

(defn test4 []
  (print "Find the pathstring of root: ")
  (println (jsonPathStr testZip)) 
  (println)
  (print "Find the pathstring of 1 down: ")
  (println (jsonPathStr (zip/down testZip)))
  (println)
  ;;
  (print "Find the pathstring of 1 down and 1 to the right: ")
  (println (jsonPathStr (zip/right (zip/down testZip))))
  (println)
  ;;	   
  (print "Find the pathstring of 2 down: ")
  (println (jsonPathStr (zip/down (zip/down testZip))))
  (println))

)