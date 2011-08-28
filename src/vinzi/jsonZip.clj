(ns vinzi.jsonZip
  (:require (clojure [zip :as zip] [walk :as walk]))
  (:require clojure.pprint)
  )



(def jsonTypeMap ::jsonMap)
(def jsonTypeVector ::jsonVector)

(defn isZipper?
  "Routine to test whether 'zipper' is a real zipper or nil. Used in pre-conditions to prevent extracted nodes from being passed as an argument when actually a zipper is needed."
  [zipper]
  (or (nil? zipper)
      (and (vector? zipper)
	   (= 2 (count zipper)))))


(defn jsonToZippertree
  "Transform an in-memory json structure (a nested hash-map/vector) to a tree that can be editted via the Zipper toolset. The nested map is not directly edittable as you can not insert children via the Edit/Replace functionality. The zipperTree resolves this by splitting each map in a set of basic elements, while all compound elements (maps and vectors) are stored in a vector with key :jsonChildren. Vectors are stored as a map with only one key { :jsonChildren  [...] }
The (original) keys and path-strings are stored in the metadata as :json/key  and :json/path.
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
					 (list :jsonChildren vc)     
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
  "Translate a vinzi.jsonzipperTree back to a hash-map (an in-memory json structure)"
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
  "Creates a jsonZipper which can be modified (processed) the functions from clojure.zip (such as up, down, left, right, insert, remove, ...) . The jsonRoot should represent a clojure map that represents a json-structure."
  [jsonRoot]
  (zip/zipper jsonBranch?
	  jsonChildren
	  jsonMakeNode
	  (jsonToZippertree jsonRoot)))

(defn jsonRoot
  "Extract the root of a jsonZipper and transform the resulting zipperTree to an in-memory json object."
  [node]
  {:pre [(isZipper? node)]}
(zippertreeToJson (zip/root node)))

;;
;; helper routines for pretty-printing data, extracting  meta-data, extracting path etc...
;;

(defn pprintJsonZipper
  "Traverses a jsonZipper structure and pretty-prints the data in the tree and the corresponding meta-data. (Meta-data is used to store the keys of the original Json-structure."
  [zipper]
  {:pre [(isZipper? zipper)]}
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
  {:pre [(isZipper? zipper)]}
  (let [node (zip/node zipper)
	sk    (sort (filter #(not (= :jsonChildren %)) (keys node)))
	sv    (map node sk)
	skv   (apply str (interpose "<br/>" (map #(format "<font color=blue>%s</font>:\t %s" (name %1) %2) sk sv)))
	html  (format "<html>%s</html>" skv)]
	html))

(defn nodeChildrenHtml
  "Show the contents of the current node as a html-string."
  [zipper]
  {:pre [(isZipper? zipper)]}
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
  {:pre [(isZipper? zipper)]}
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
  {:pre [(isZipper? zipper)]}
  (:json/type (meta (zip/node zipper))))

(defn jsonStatusNode
  "Return the key of the current node in the jason-zipperTree"
  [zipper]
  {:pre [(isZipper? zipper)]}
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
  ([node]
     {:pre [(isZipper? node)]}
     (jsonPathList node '() ))
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



(defn zipError
  ([msg] (zipError msg nil))
  ([msg ret]
     (println "ERROR: " msg)
     ret))

(defn zipTop [z]
  (if-let [u (zip/up z)]
    (recur u)
    z))

(defn getKey
  ;; translate a key to an integer if it is a vector-key ("[n]")
  [key]
  (if (and (not= (type key) (type :a)) 
	   (= (first key) \[))
    (Integer/parseInt (subs key 1 (dec (count key))))
    key))


(defn findChildWithKey
  ;; find the key within the current level of the zipper
  [z k]
  (if z
    (loop [z (zip/down z)]
      (if (or (nil? z)
	      (= (jsonKey z) k))
	z
	(recur (zip/right z))))
    (zipError (str "key " k " could not be located")))) ;; return nil


(defn zipLoc
  ;; find the location (starting at the root)
  [z pl]
  (zipTop z)
  (assert (or (= (first pl) "/") (empty? pl)))
  (loop [[k ks] (if (seq pl) (rest pl) pl)
	 z z]
    (if (or (nil? z)
	    (nil? k))
      z   ;; arrived at location (or nil)
      (recur (findChildWithKey z k) ks))))


(defn processCompoundOper
  "Process an operation 'oper' for a compound object with key 'key' that is a direct child of 'loc'."
  [loc key oper & args]
  (if-let [newLoc (findChildWithKey loc key)]
    (let [arguments (if args
		      (concat (list newLoc) args)
		      (list newLoc))]
      (println "apply" oper " to newLoc = " newLoc " with args " arguments) (flush)
      (apply oper arguments))
    loc)) ;; failed


(defn deleteItem
  "Delete a field from zipper 'loc' at location 'pathlist'/'key'. "
  [loc pathList key]
  (if-let [newLoc (zipLoc loc pathList)]
    (if (= (jsonType newLoc) jsonTypeMap)
      (let [node (zip/node newLoc)]
	(if (get node key)
	  ;; key is a basic key (otherwise compound key(-field))
	  (zip/replace newLoc (dissoc node key))
	  (processCompoundOper newLoc key zip/remove)))
      (processCompoundOper newLoc key zip/remove))
    nil))  ;; search failed, so return loc unmodified


(defn insertItem
  "Insert a field in zipper 'loc' at location 'pathlist'/'key' with contents given by 'json'. This function differentiates between simple and compound values (Compound values will be added as a child)."
  [loc pathList key json]
		      (println "Insert")
		      (if-let [newLoc (zipLoc loc pathList)]
			(let [node (zip/node newLoc)]
			  (if (or (get node key)
				  (findChildWithKey newLoc key))
			    (zipError (str "node with key " key " exists. Insert failed"))
			    (if (coll? json)
			      (let [item (setJsonKey key (jsonToZippertree json))]
				(zip/append-child newLoc item))
			      ;; it is a basic type
			      (if (= (jsonType newLoc) jsonTypeMap)
				(zip/replace newLoc (assoc node key json))
				(let [boxed (with-meta (list json) {:json/boxed true})
				      item (setJsonKey key boxed)]
				  (zip/append-child newLoc item))))))
			nil))  ;; search failed, so return nil

(defn replaceItem
  "Insert a field in zipper 'loc' at location 'pathlist'/'key' with contents given by 'json'. This function differentiates between simple and compound values (Compound values should be handled as a child). Also it needs to notice type-changes and take the appropriate action."
  [loc pathList key json]
  (if-let [newLoc (deleteItem loc pathList key)]
    (insertItem loc pathList key json)
    nil))

  ;; (println "Change: replace by check, delete, insert cycle. (automatic type conversion)")
  ;; 		      (if-let [newLoc (zipLoc loc pathList)]
  ;; 			(if (= (jsonType newLoc) jsonTypeMap)
  ;; 			  (let [node (zip/node newLoc)]
  ;; 			    (if (get node key)
  ;; 			      ;; key is a basic key (otherwise compound key(-field))
  ;; 			      (zip/replace newLoc (assoc node key json))
  ;; 			      (processCompoundOper newLoc key zip/replace json)))
  ;; 			  (processCompoundOper newLoc key zip/replace json))
  ;; 			nil))  ;; search failed, so return nil

