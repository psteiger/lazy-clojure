(defn count-consec
  [coll]
  (loop [coll   coll
	 result 1]
    (cond
      (empty? coll)                     0
      (not= (first coll) (second coll)) result
      :else                             (recur (rest coll) (inc result)))))

(defn remove-consec
  "Same as above, but uses functional programming"
  [coll]
  (loop [coll coll 
         e    (first coll)]
    (cond (empty? coll) 
	    ""
	  (not= (first coll) e)
	    coll
	  :else
            (recur (rest coll) e))))

(defn morrizise
  [coll]
  (loop [coll   coll 
         result ""]
    (if (empty? coll) result
	(recur (remove-consec coll)
	       (str result
		    (count-consec coll) 
		    (first coll))))))

(defn lazy-morris
  [start]
  (letfn [(next [prev]
		(let [new (morrizise prev)]
		  (lazy-seq (cons new (next new)))))]
    (lazy-seq (cons (str start) (next (str start))))))