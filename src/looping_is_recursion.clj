(ns looping-is-recursion)

(defn power [base exp]
  (loop [acc 1
		 base base
		 exp exp]
	(if (zero? exp)
	  acc
	  (recur (* acc base) base (dec exp)))))

(defn last-element [a-seq]
  (loop [last-elem nil
		 a-seq a-seq]
	(if (empty? a-seq)
	  last-elem
	  (recur (first a-seq) (rest a-seq)))))

(defn seq= [seq1 seq2]
  (cond
   (empty? seq1) 					(empty? seq2)
   (empty? seq2) 					false
   (not= (first seq1) (first seq2)) false
   :else 							(recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [index 0
		 pred pred
		 a-seq a-seq]
	(cond
	 (empty? a-seq) 		nil
	 (pred (first a-seq)) 	index
	 :else 					(recur (inc index) pred (rest a-seq)))))

(defn avg [a-seq]
  (if (empty? a-seq)
	0
	(loop [counter 0
		   acc 0
		   a-seq a-seq]
	  (if (empty? a-seq)
		(/ acc counter)
		(recur (inc counter) (+ acc (first a-seq)) (rest a-seq))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
	  (disj a-set elem)
	  (conj a-set elem)))

(defn parity [a-seq]
  (loop [acc #{}
		a-seq a-seq]
	(if (empty? a-seq)
	  acc
	  (recur (toggle acc (first a-seq)) (rest a-seq)))))

(defn fast-fibo [n]
  (if (= n 0)
	0
	(loop [fibk-1 0
		   fibk 1
		   k 1
		   n n]
	  (if (= k n)
		fibk
		(recur fibk (+ fibk-1 fibk) (inc k) n)))))

(defn cut-at-repetition [a-seq]
  (loop [elems #{}
		 acc []
		 a-seq a-seq]
	(if (or (empty? a-seq) (contains? elems (first a-seq)))
	  acc
	  (recur (conj elems (first a-seq)) (conj acc (first a-seq)) (rest a-seq)))))
