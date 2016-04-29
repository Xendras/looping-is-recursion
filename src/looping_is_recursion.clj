(ns looping-is-recursion)

(defn power [base exp]
  (let [helper-power (fn [acc k]
                       (if (zero? k)
                         acc
                         (recur (* acc base) (dec k))))]
    (helper-power 1 exp)))

(defn last-element [a-seq]
  (let [helper-last (fn [acc seq]
                      (if (<= acc 1)
                        (first seq)
                        (recur (dec acc) (rest seq))))]
    (helper-last (count a-seq) a-seq)))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (= (first seq1) (first seq2))
      (recur (rest seq1) (rest seq2))
    :else false))

(defn find-first-index [pred a-seq]
  (loop [n 0]
    (cond
      (>= n (count a-seq)) nil
      (pred (get a-seq n)) n
      :else (recur (inc n)))))

(defn avg [a-seq]
  (loop [n 0 acc 0]
    (if (>= n (count a-seq))
      (/ acc (count a-seq))
      (recur (inc n) (+ acc (get a-seq n))))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [seq a-seq set #{} ]
    (if (empty? seq)
      set
      (recur (rest seq) (toggle set (first seq))))))

(defn fast-fibo [n]
  (loop [m 0 sum-n 1 sum-n-1 0 ]
    (cond
      (zero? n) 0
      (== 1 n) 1
      (== m n) sum-n-1
      :else (recur (inc m) (+ sum-n-1 sum-n) sum-n ))))

(defn cut-helper [a-set]
  (loop [seq () n 0 set a-set]
    (if (>= n (count a-set))
      seq
      (recur (conj seq (first set)) (inc n) (rest set)))))

(defn cut-at-repetition [a-seq]
  (loop [checked [] seq a-seq]
    (cond
      (empty? seq) a-seq
      (some #{(first seq)} checked) checked
      :else (recur (conj checked (first seq)) (rest seq)))))

