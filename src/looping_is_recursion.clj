(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) (dec exp))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc a-seq]
                 (if (empty? a-seq)
                   acc
                   (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (cond 
    (not (= (count seq1) (count seq2))) false
    (and (empty? seq1) (empty? seq2)) true
    (not (= (first seq1) (first seq2))) false
    :else (seq= (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (let [helper (fn [acc pred a-seq]
                 (cond
                   (empty? a-seq) nil
                   (pred (first a-seq)) acc
                   :else (recur (inc acc) pred (rest a-seq))))]
    (helper 0 pred a-seq)))

(defn avg [a-seq]
  (let [helper 
        (fn [average num-items a-seq]
          (cond
            (empty? a-seq) average
            :else (recur (/ (+ (first a-seq) (* num-items average)) 
                               (inc num-items)) 
                         (inc num-items) (rest a-seq))))]
    (helper 0 0 a-seq)))

(defn parity [a-seq]
  (map first (filter #(odd? (second %)) (frequencies a-seq))))

(defn recur-fibo [n]
  (if (<= n 1)
    n
    (+ (recur-fibo (dec (dec n))) (recur-fibo (dec n)))))

(defn fast-fibo [n]
  (let [new-acc
        (fn [acc]
           (conj acc (+ (acc (dec (count acc))) (acc (- (count acc) 2)))))]
    (loop [acc [0 1]
           m 0]
      (cond 
        (= m n) (acc m)
        (= m 0) (recur acc (inc m))
        (= m 1) (recur (new-acc acc) (inc m))
        :else (recur (new-acc acc) (inc m))))))

(defn my-contains? [a-seq target]
  (not (nil? (some #(= % target) a-seq))))

(defn cut-at-repetition [a-seq]
  (loop [keepers []
         b-seq a-seq]
    (cond 
      (empty? b-seq) keepers
      (and (> (count keepers) 0)
           (my-contains? keepers (first b-seq))) keepers
      :else (recur (conj keepers (first b-seq)) (rest b-seq)))))

