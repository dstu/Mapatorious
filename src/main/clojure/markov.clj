(defn make-markov-hash [ht toAdd]
  (let [entry (get ht (first toAdd))]
    (if entry
      (assoc ht (first toAdd) (conj entry (fnext toAdd)))
      (assoc ht (first toAdd) (vector (fnext toAdd))))))

(defn get-grams [str len]  
  (let [strp (concat "@" (concat str "$"))
        buffer (if (> len 2) 
                   (map (fn [x] (take x strp)) (range 2 len))
                   '())]
    (concat buffer
      (map 
        (fn [x] (take len (drop x strp))) 
        (range 0 (count str))))))

(defn prefix-suffix [len]
  (fn [str]
    (let [slen (count str)]
      (if (> len slen)
          (list (take len str) (drop len str))
          (list (take (- slen 1) str) (drop (- slen 1) str))))))

(defn add-to-hash [ht str]
  (let [grams (get-grams str 3)]
    (reduce make-markov-hash 
            ht 
            (map (prefix-suffix 2) grams))))


(defn build-corpus [slist]
  (reduce add-to-hash {} slist))

(defn drop-newline [ls]
  (filter #(not= %1 '(\newline)) ls))

(defn file-to-corpus [file]
  (reduce add-to-hash {} 
    (drop-newline (partition-by #(= %1 \newline) (slurp file)))))

(defn generate-name [corpus]
  (apply str
  (let [start (rand-nth (get corpus '(\@)))] ;; pull down the start symbol
    (loop [acc start
           key (cons \@ start)]
      (if key
        (let [v (get corpus key)]
          (if v
            (let [char (rand-nth v)]
              (recur (concat acc char) (cons (last acc) char)))
            (butlast acc)))
        (butlast acc))))))
    
(def england (file-to-corpus "england"))
(def germany (file-to-corpus "german"))
(def aus (file-to-corpus "australia"))
