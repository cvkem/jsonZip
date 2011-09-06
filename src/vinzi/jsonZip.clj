(ns vinzi.jsonZip
  (:require (clojure [zip :as zip]
		     [walk :as walk]
		     [string :as str :only [lower-case]]))
  (:use clojure.pprint)
  )


(defmacro dbgi [x] `(let [x# ~x] (do (println "INS " '~x "=" x#) (flush)) x#))
(defmacro dbgmi [m x] `(let [x# ~x] (do (println "INS " ~m " : " '~x "=" x#) (flush)) x#))
(defmacro dbgmr [m x] `(let [x# ~x] (do (println "REMOVE "~m " : " '~x "=" x#) (flush)) x#))

;;(defmacro dbgmi [m x] x)
(defmacro dbgmr [m x] x)




;;;;;;;;;;;;;;;;;;
;;  declarations of types and some of the helper functions
;;

(def jsonTypeMap ::jsonMap)
(def jsonTypeVector ::jsonVector)

(def allowVectorId true)

(def vectorId  "id")





;;;;;;;;;;
;;  helper functions to report errors
;;

(def zipErrs (atom []))

(defn zipError
  ([msg] (zipError msg nil))
  ([msg ret]
     (println "ERROR: " msg)
     ;; add to error-queue
     (swap! zipErrs conj msg)
     ret))

(defn getZipErrors []
  @zipErrs)

(defn clearZipErrors []
  (swap! zipErrs (fn [_ _] []) nil))

(declare isZipper?)
(declare isBoxed?)
(declare boxWithMeta)
(declare zipTop)





;;;;;;;;;;
;;  core functions to generate zipper-trees from json-object and vice versa.
;;

(defn jsonToZippertree
  "Transform an in-memory json structure (a nested hash-map/vector) to a tree that can be editted via the Zipper toolset. The nested map is not directly edittable as you can not insert children via the Edit/Replace functionality. The zipperTree resolves this by splitting each map in a set of basic elements, while all compound elements (maps and vectors) are stored in a vector with key :jsonChildren. Vectors are stored as a map with only one key { :jsonChildren  [...] }
The (original) key is stored in the metadata with key-label :json/key. The key :json/vectId is set to an 'id' if the vector is index by this 'id' instead of the slot-index."
   ([node]
      (jsonToZippertree node {:json/key "/"}))
   ([node metadata]
      (let [isCompound (fn [kv]
			 (let [v (second kv)]
			   (and (coll? v)
				(not (isBoxed? v)))))
	    visitMap (fn [node metadata]
		       (let [k (keys node)
			     v (vals node)
			     mn (meta node)
			     childMeta (if mn
					 (map #(into mn {:json/key %}) k)  ;; k should overwrite old keys
					 (map #(hash-map :json/key %) k)) 
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
	    getVectIndex   (fn [node metadata]
			     (map #(into metadata {:json/key (str "[" % "]")} )
					     (range (count node)))) ;add key (index) to metadata
	    getVectMetas (fn [node metadata]
			     (if (or (not allowVectorId) (< (count node) 1))
			       (getVectIndex node metadata)
			       (let [cn (count node)
				     ids (set (map #(get % vectorId) node)) ;; one nil-key allowed!!
				     cid (count ids)]
				 (if (and (= cid 1) (= (first ids) nil))  ;; only nil keys
				   (getVectIndex node metadata)
				   (if (< cid cn)    ;; not sufficient keys (one nil allowed)
				     (do
				       (when (> cid 1)
					 (zipError (format
					  "only %s keys for vector of size %s (keys discarded)" cid cn)))
				       (getVectIndex node metadata))
				     (map #(into metadata {:json/key (str (get % vectorId))
							   :json/vectId vectorId} ) node))))))
	    visitVector (fn [node metadata]
			  (let [metas   (getVectMetas node metadata)
				jsonChildren (vec (map jsonToZippertree node metas))   ;; change LIST to VECTOR
				jsonChildrenMeta (with-meta jsonChildren metadata)
				theMetadata (into metadata {:json/type jsonTypeVector})
				result  (with-meta {:jsonChildren jsonChildrenMeta} theMetadata)]
			    result
			    ))
	    ;; boxWithMeta (fn [node metadata]
	    ;; 		  (if (contains? (parents (class node)) clojure.lang.IObj)
	    ;; 		    (with-meta (into metadata (meta node)))
	    ;; 		    (with-meta (list node) (into {:json/boxed true} metadata))))
	    ]
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
      (if (and (isBoxed? node)
	       (list? node)
	       (= 1 (count node)))
	(first node)
	node))))  





;;;;;;;;;;
;; functions needed to generate the 'Huet' zipper on Zipper-trees (and thus on json-objects.
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
  (if children
    ;;   don't put meta-data on the jsonChildren vector. Use surrounding map.
    ;;    (assoc node :jsonChildren (with-meta children (meta node)))
    (assoc node :jsonChildren children)
    (dissoc node :jsonChildren [])))


(defn jsonZipper
  "Creates a jsonZipper which can be modified (processed) the functions from clojure.zip (such as up, down, left, right, insert, remove, ...) . The jsonRoot should represent a clojure map that represents a json-structure."
  [jsonRoot]
  (zip/zipper jsonBranch?
	  jsonChildren
	  jsonMakeNode
	  (jsonToZippertree jsonRoot)))





;;;;;;;;;;;;;;;;;
;; helper routines for pretty-printing data, extracting  meta-data, extracting path etc...
;;

(defn pprintJsonSubZipper
  "Traverses a jsonZipper structure and pretty-prints the data in the tree and the corresponding meta-data. (Meta-data is used to store the keys of the original Json-structure."
  ([zipper]
     {:pre [(isZipper? zipper)]}
     (pprintJsonSubZipper zipper "R"))
  ([zipper prefix]
     (let [numChild (if (zip/branch? zipper)
		      (count (zip/children zipper)) -1)
	   node    (zip/node zipper)
	   metaData (meta node)
	   base     (if (map? node)
		      (dissoc node :jsonChildren)
		      (if (isBoxed? node)
			(first node)
			node))]
       (print prefix " BASE: ")
       (pprint base)
       (print prefix " WITH META: ")
       (pprint metaData)
       (when (>= numChild 0)
	 (println prefix " NOW processing the " numChild " children")
	 (flush)
	 (loop [nr 1
		child (zip/down zipper)]
	   (when child
	     (pprintJsonSubZipper child (str prefix "." nr))
	     (recur (inc nr) (zip/right child))))
	 (println)))))

(defn pprintJsonZipper
  "Traverses a jsonZipper structure starting at the top of the tree and pretty-prints the data in the tree and the corresponding meta-data. (Meta-data is used to store the keys of the original Json-structure."
  [zipper]
  {:pre [(isZipper? zipper)]}
  (pprintJsonSubZipper (zipTop zipper)))


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
	chk  (map #(:json/key (meta %)) chdr)
	cht  (map #(let [jt (:json/type (meta %))
			 jt (if (nil? jt) (type %) jt)] jt) chdr)
	list_kv (interpose "<br/>" (map #(format "<font color=blue>%s</font>:\t %s" (name %1) %2) chk cht))]
    (if (seq list_kv)
      (do
	(format "<html>%s</html>" (apply str list_kv)))
      "")))   ;; If no children return "" 





;;;;;;;;;;
;; functions to extract and change the meta-data (data about the structures
;;

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
  (if (list? data) 
    (if (seq data)
      (let [sData (map #(if (= (type %) (class 1)) (str "[" % "]") %) data) ;; map integer-keys to [i]
	  res   (apply str (cons (first sData) (interpose "/" (rest sData))))] ;; treat '/' separate
	res)
      "NONE")
    (jsonPathStr (jsonPathList data))))  ;; data is a node. Extract it's pathList

(defn getKey
  ;; translate a key to an integer if it is a vector-key ("[n]")
  [key]
  (if (and (not= (type key) (type :a)) 
	   (= (first key) \[))
    (Integer/parseInt (subs key 1 (dec (count key))))
    key))

(defn jsonKey
  "Return the key of the current node in the jason-zipperTree"
  [zipper]
  {:pre [(isZipper? zipper)]}
  (:json/key (meta (zip/node zipper))))

(defn setJsonKey
  "Set a :json/key and :json/type for an element while preserving the existing metadata.
This function is only used for compound elements (collections) that will be inserted in a hashmap. If necessary the element will be boxed (put in a list) in order to allow attachement of metadata."
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
	elemMeta  (meta element)
	metaData  (into keyMeta typeMeta)
	metaData  (if elemMeta  (into elemMeta metaData) metaData)]  ;; keyMeta and typeMeta should override elemMeta 
  (with-meta element metaData)))

(defn jsonType
  "Return the type of the current node in the jason-zipperTree  (jsonTypeMap or jsonTypeVector)."
  [zipper]
  {:pre [(isZipper? zipper)]}
  (:json/type (meta (zip/node zipper))))

(defn getVectId
  "Return the vectorId (or nil if it is not set)."
  [zipper]
  {:pre [(isZipper? zipper)]}
  (:json/vectId (meta (zip/node zipper))))

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

(defn isBoxed? [v]
  (:json/boxed (meta v)))

(defn boxWithMeta [node metadata]
  (if (contains? (parents (class node)) clojure.lang.IObj)
    (with-meta (into metadata (meta node)))
    (with-meta (list node) (into {:json/boxed true} metadata))))






;;;;;;;;;;
;; Functions to navigate, check and modify the zipper-trees/json-objects.
;;

(defn isZipper?
  "Routine to test whether 'zipper' is a real zipper or nil. Used in pre-conditions to prevent extracted nodes from being passed as an argument when actually a zipper is needed."
  [zipper]
  (or (nil? zipper)
      (and (vector? zipper)
	   (= 2 (count zipper)))))

(defn jsonRoot
  "Extract the root of a jsonZipper and transform the resulting zipperTree to an in-memory json object."
  [node]
  {:pre [(isZipper? node)]}
  (zippertreeToJson (zip/root node)))

(defn zipTop
  "Move the zipper to the top of the tree (materializing all changes)"
  [z]
  (if-let [u (zip/up z)]
    (recur u)
      z))

(defn findChildWithKey
  ;; find the key within the current level of the zipper
  ([z k]  (findChildWithKey z k true))
  ([z k repErr]
  (if z
    (loop [z (zip/down z)]
      (if (or (nil? z)
	      (= (jsonKey z) k))
	z
	(recur (zip/right z))))
    (if repErr ;; return nil
      (zipError (str "key " k " could not be located"))
      nil)))) 

(defn zipLoc
  ;; find the location (starting at the root)
  [z pl]
  (assert (or (= (first pl) "/") (empty? pl)))
  (loop [z (zipTop z)
	 [k ks] (if (seq pl) (rest pl) pl)]
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
      (apply oper arguments))
    nil)) ;; failed

(defn keyExistsAt
  "Checks whether a key exists at location 'loc' (either hash-key or child key). This function checks for the keywordized and string version of key."
  [loc key]
  (let [node (zip/node loc)
	keywordComplement (fn [id]
			    ;; the keywordComplement of id ':name' is
			    ;; 'name' and vice versa.
			    (if (keyword? id) (name id) (keyword id)))
	keyExistsAux  (fn [loc key]
		       ;; check whether key exists
		       (or (get node key)
			   (findChildWithKey loc key false))) ]  ;; find child without reporting error on fail.
    (let [res (if (keyExistsAux loc key)
		true ;; key exists
		(keyExistsAux loc (keywordComplement key)))]
      (println "RETURN VALUE of keyExists at for " key ": "  res)
      res)))

(defn removeItem
  "Removes a key field from zipper 'loc' at location 'pathlist'/'key'. Key can refer to a child, or to a basic element of a map."
  [loc pathList key]
  (if-let [newLoc (dbgmr "parent: " (zipLoc loc pathList))]
    (if (dbgmr "map?: " (= (jsonType newLoc) jsonTypeMap))
      (let [node (zip/node newLoc)]
	(if (get node key)
	  ;; key is a basic key (otherwise compound key(-field))
	    (dbgmr "result (replace basic map-key): " (zip/replace newLoc (dissoc node key)))
	    (dbgmr "result (replace compound key): " (processCompoundOper newLoc key zip/remove))))
	(dbgmr "res=non-map item" (processCompoundOper newLoc key zip/remove)))
    nil)) ;; failed

(defn insertItem
  "Insert a field in zipper 'loc' at location 'pathlist'/'key' with contents given by 'json'. This function differentiates between simple and compound values (Compound values will be added as a child)."
  [loc pathList key json]
		      (if-let [newLoc (dbgmi "parentLoc" (zipLoc loc pathList))]
			(let [vectId (getVectId newLoc)
			      vectKey (if (and vectId (coll? json))
					(json vectId) nil)
			      _    (when (and vectKey (not= vectKey key))
				     (zipError (format
					"vector uses field '%s' as id. Key  '%s' replaced by '%s'"
					vectId key vectKey)))
			      key (if vectKey  vectKey key)
			      ]
			  (if (keyExistsAt newLoc key)
			    (dbgmi (str  key " exists?:) ") (zipError (str "node with key " key " exists. Insert failed")))
			    (if (coll? json)
			      (if (dbgmi "map has children" (zip/branch? newLoc))
				;; add compound type as an additional child
				(let [;;item (setJsonKey key (jsonToZippertree json))]
				      item (jsonToZippertree json {:json/key key})]
				  (dbgmi "res=add compound" (zip/append-child newLoc item)))
				;; loc has no children yet (so replace full tree)
				(let [node    (zip/node newLoc)
				      jsonNode   (zippertreeToJson node)   ;; step is redundant
				      newNodeContents  (assoc jsonNode key json)
				      newNode (dbgmi "newNode with insert " (jsonToZippertree newNodeContents))] ;; generates key
				  (dbgmi "res=insert first-child" (zip/replace newLoc newNode))))
			      ;; it is a basic type
			      (let [node (zip/node newLoc)]
				(if (= (jsonType newLoc) jsonTypeMap)
				  (dbgmi "res=basic type in map: " (zip/replace newLoc (assoc node key json)))
				  (let [item (setJsonKey key json)]
				    (dbgmi "res=basic type on non-map" (zip/append-child newLoc item))))))))
			nil))  ;; search failed, so return nil

(defn zipReplace [z]
  (println "Replace needs to be investigated to see whether it preserves keys"))

(defn replaceItem
  "Insert a field in zipper 'loc' at location 'pathlist'/'key' with contents given by 'json'. This function differentiates between simple and compound values (Compound values should be handled as a child). Also it needs to notice type-changes and take the appropriate action."
  [loc pathList key json]
  (if (= (count pathList) 0)
    (jsonZipper json)   ;; a full replacement (root is replaced)
    (if-let [newLoc (removeItem loc pathList key)]
      (insertItem newLoc pathList key json)
      nil)))  ;; for arrays this implies that replace results in an append (insert is not provided)


