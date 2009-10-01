(defn fib [n]
  (cond (= n 0) 0
	(= n 1) 1
	:else (+ (fib (- n 1))
		 (fib (- n 2)))))

(defn lazy-seq-fib-2
  []
  (letfn [(next [x y]
		(lazy-seq (cons (+ x y)
				(next y (+ x y)))))]
    (concat [0 1] (next 0 1))))


(defn elev
  ([a]
     (lazy-seq (cons a (elev (* 2 a)))))
  ([a b]
     (take b (elev a))))
 
(defn lazy-seq-fib
  ([]
     (concat [0 1] (lazy-seq-fib 0 1)))
  ([a b]
     (let [n (+ a b)]
       (lazy-seq
	 (cons n (lazy-seq-fib b n))))))

(defn lazy-mod-list
  [n]
  (letfn [(step [a] (lazy-seq (cons a (step (+ a n)))))]
    (cons 0 (step n))))

(defn f2ilter
  "Returns a lazy sequence of the items in coll for which
  (pred item) returns true. pred must be free of side-effects."
  [pred coll]
  (let [step (fn [p c]
                 (when-let [s (seq c)]
                   (if (p (first s))
                     (cons (first s) (filter p (rest s)))
                     (step p (rest s)))))]
    (lazy-seq (step pred coll))))