(defstruct -target :node :dist)
(defstruct -node :label :targets)

(def network (ref {}))
(defn create-node
  [{nodes :neighbors
    label :label}]
  nodes)

(defn create-node
  [node & targets]
  (dosync
   (ref-set network (assoc @network node targets))
   (loop [t targets]
     (let [x (first t)]
       (prn x)
       (if (not (nil? x))
	 (let [newnode (first x)
	       dist (second x)]
	   (dosync
	    (ref-set network (assoc @network newnode (conj (newnode @network) [node dist]))))
	   (recur (rest t))))))))

[1 [2 3] [4 5]]
[2 [1 3]]
  