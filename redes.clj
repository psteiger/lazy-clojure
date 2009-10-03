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

(defn neighbors
  "Returns a list of the neighbors of node in the network. No side-effects"
  [node]
  (keys (node @network)))

(defn shortest-path
  [source]
  (loop [distances {
{:1 {:2 3 :4 5}}
 :2 {:1 3}
 :4 {:1 5}}
para cada vizinho,
{vizinho {2
{:2 {:1 3}
 :4 {:1 5}}
(def dist-map (ref {}))
(defn update-neighbors
  "Updates the status of neighbors within network.
  A updated key in the dist-map looks like:
  :neighbor [n d], where:
  neighbor is a neighbor of node, 
  n is the node, and
  d is the distance within node and neighbor"
  [node]
  (mapcat #(list 1 [(dist-between node %1) node])
	  (neighbors node)))