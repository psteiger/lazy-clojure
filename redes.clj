(defstruct -target :node :dist)
(defstruct -node :label :targets)

(def network (ref {}))

(defn add-to-network
  "Adds a node (a keyword) associated with a target map. The targets
should be in the form: {:label1 distance1 :label2 distance2}" 
  [node targets]
  (let [new-targets (merge (node @network) targets)] ; merge old targets with new targets
    (prn node targets new-targets)
    (dosync (ref-set network (assoc @network node new-targets))))) ; associate the result with node
 
(defn create-node
  [node targets]
  (add-to-network node targets) ; add actual node and targets to network
  (loop [t-map targets t (first targets)] ; loop over targets in target-list
    (prn t "t") 
    (if (not (nil? t))
      (let [newnode (first t)     ; get the target node and dist of each target
	    dist    (second t)]
	(prn newnode dist "newnode dist")
	(add-to-network newnode {node dist}) ; add target node with target {node dist}
	(recur (rest t-map) (first (rest t-map)))))))

(defn key-by-value
  [map v]
  (cond (empty? map) nil
	(= v (val (first (seq map)))) (key (first (seq map)))
	:else (recur (rest map) v)))

(defn get-neighbors
  [node]
  (keys (node @network)))

(defn dist-between
  [u v]
  (u (v @network)))

(defn infinite?
  [val]
  (= val Double/POSITIVE_INFINITY))

(defn del
  [coll e]
  (filter #(not= %1 e) coll))

(defn max-subseq
  [subcoll coll]
  (cond
    (empty? subcoll) empty
    (some #{(first subcoll)} coll) (cons (first subcoll) (max-subseq (rest subcoll) coll))
    :else (max-subseq (rest subcoll) coll)))

(defn dijkstra-search
  "Dijkstra algorithm, in procedural style (functional Dijkstra
  was too much difficult to implement)"
  [source]
  (let [node-lst (ref (keys @network)) 
	dist (ref (zipmap @node-lst       (for [x @network] Double/POSITIVE_INFINITY)))
        src  (ref (zipmap (keys @network) (for [x @network] nil)))]
    (pr "Setting dist from: " @dist)
    (dosync (ref-set dist (merge @dist {source 0})))     ; set dist[source] to 0
    (prn @dist)
    (while (not (empty? @node-lst))	          ; while node-lst is not empty
      (prn "Processing node-lst. Now: " @node-lst)
      (prn "Src is:" @src)				
      (let [u (key-by-value @dist
			    (apply min (map #(%1 @dist) @node-lst)))] ; u := key in node-lst that maps to the min value in dist
	(prn "Key 'u' in node-list that maps to the min value in dist:" u)
	(prn "Removing u from node-lst. node-lst now:")
	(dosync (ref-set node-lst (del @node-lst u)))
	(prn @node-lst)
	(if (infinite? (u @dist))    ; if this key dist to source is infinite
	  (do (prn "Key is infinite:" u) @dist) ; all other vertices are inaccessible from src
	  (do ; remove u from node-lst
	   (loop [v (get-neighbors u)]
	      (prn "V is" v) 		; for each neighbor v of u
	      (cond (empty? v)
		    @dist
		    (for [v v] v)
		    (do
		      (for [v v] ; if v still exists in node-
			(do (prn "V is: " v)
			  (if (some #{v} @node-lst)
			  (let [alt (+ (u dist) (dist-between u v))]
			    (prn "alt" alt)
			    (if (< alt (v dist))
			      (dosync
			       (ref-set dist (merge @dist {v alt}))
			       (ref-set src (merge @src {v u}))))
			    (prn "new dist src" @dist @src))))
		      (recur (rest v))))))))))))