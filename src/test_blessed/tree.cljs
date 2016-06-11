(ns test-blessed.tree)

(defn make-leaf [e d]
  (with-meta [e d] {:leaf? true}))

(defn leaf? [n]
  (:leaf? (meta n)))

(defn sum [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn less? [[o1 _] [o2 _]]
  (< o1 o2))

(defn make-tree []
  ['() [0 0]])

(def thresh 3)

(defn split-subtree [children data]
  (let [c (count children)]
    (if (> c thresh)
      (let [[left right] (split-at (quot c 2) children)
            sub-sum #(reduce sum (map second %))]
        [[left (sub-sum left)] [right (sub-sum right)]])
      [[children data]])))

(defn insert
  ([t s offset len]
   (let [[ins & [rest]] (insert t s offset len :impl)]
     (if rest
       [[ins rest] (sum (second t) [len 1])]
       ins)))
  ([[children data] s offset len _]
   (let [[idx acc c] (reduce (fn [[idx acc _] [_ [off _] :as c]]
                               (if (< offset (+ off acc))
                                 (reduced [idx acc c])
                                 [(inc idx) (+ off acc) c]))
                             [0 0 nil]
                             children)]

     (let [[low high] (split-at idx children)]
       (-> (concat low (if (or (leaf? c) (nil? c))                         
                         [(make-leaf s [len 1])]
                         (insert c s (- offset acc) len :impl))
                   (if (or (leaf? c) (nil? c))                         
                     high
                     (rest high)))
           (split-subtree (sum data [len 1])))))))

(defn intersects? [[from1 to1] [from2 to2]]
  (< (max from1 from2) (min to1 to2)))

(comment
  (intersects? [1 5] [2 3])
  (intersects? [1 5] [3 10])
  (intersects? [1 5] [5 6]))

(defn contains-range? [[from1 to1] [from2 to2]]
  (and  (<= from1 from2) (<= to2 to1)))

(comment
  (contains-range? [1 5] [2 3])
  (contains-range? [1 5] [3 5])
  )

(defn query
  ([tree from to select] (query tree [0 0] from to select))
  ([[children data :as tree] acc from to select]
   (if (not (leaf? tree))
     (second
      (reduce (fn [[acc res] c]
                (let [sel (select (second c))
                      acc-sel (select acc)]
                  [(sum acc (second c))
                   (if (intersects? [from to] [acc-sel (+ acc-sel sel)])
                     (concat res (query c acc from to select))
                     res)]))
              [acc []]
              children))
     [[children acc]])))

(defn merge-subtrees [[children data :as tree]]
  (if (<= (reduce (fn [acc [c _]]
                   (+ acc (count c)))
                 0 children)
         thresh)
    [(mapcat
      (fn [[children _ :as c]]
        (if (leaf? c)
          [c]
          children))
      children)
     data]
    tree))

(defn delete
  ([tree from to select] (delete tree 0 from to select))
  ([[children data :as tree] acc from to select]
   (if (leaf? tree)
     tree
     (let [[_ children]
           (reduce (fn [[acc res] c]
                     (let [sel (select (second c))]
                       [(+ acc sel)                     
                        (cond
                          (contains-range? [from to] [acc (+ acc sel)])
                          res
                          
                          (intersects? [from to] [acc (+ acc sel)])
                          (conj res (delete c acc from to select))

                          :default
                          (conj res c))]))
                   [acc []]
                   children)]
       (merge-subtrees
        [children (reduce (fn [acc [_ s]]
                            (sum acc s))
                          [0 0]
                          children)])))))

(defn insert-str [tree str offset]
  (insert tree str offset (count str)))

(comment

  (-> (make-tree)
      (insert-str "abc" 0)
      (insert-str "aa" 3)
      (delete 0 3 first)
      (insert-str "abcdefaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" 0))

  
  (-> (make-tree)
      (insert "abc" 0 3)
      (insert "bcd" 3 3)
      (insert "123" 3 3)
      (insert "asddgdfgdfg" 9 11)
      (insert "22" 6 2)
      (delete 3 8 first)
      )
  
  (-> (make-tree)
      (insert "abc" 0 3)
      (delete 0 10 first)
      )

  (-> (make-tree)
      (insert-str "abc" 0)
      (insert-str "bcd" 3)
      (insert-str "cde" 3)
      (insert-str "edf" 3)
      (insert-str "dfg" 3)
      (insert-str "edf" 3)
      (insert-str "dfg" 3)
      (insert-str "edf" 3)
      (insert-str "dfg" 3)
      

      )
  
  (-> (make-tree)
      (insert "abc" 0 3)
      (insert "bcd" 3 3)
      (insert "123" 3 3)
      (insert "asddgdfgdfg" 9 11)
      (insert "22" 6 2)
      (delete 3 8 first)
      )

  (-> (make-tree)
      (insert "abc" 0 3)
      (insert "bcd" 3 3)
      (insert "123" 3 3)
      (insert "asddgdfgdfg" 9 11)
      (insert "22" 6 2)
      (delete 3 800 first)
      )

  (-> (make-tree)
      (insert "abc" 0 3)
      (insert "bcd" 3 3)
      (insert "123" 3 3)
      (insert "asddgdfgdfg" 9 11)
      (insert "22" 6 2)
      (query  6 700 first)
      )

  (-> (make-tree)
      (insert "abc" 0 3)
      (insert "bcd" 3 3)
      (insert "123" 3 3)
      (insert "asddgdfgdfg" 9 11)
      (insert "22" 6 2)
      (delete 1 2 second)
      ) 
  )
(comment

  (prn-lines @model-ptr)
  
  (-> (make-tree)
      (insert-str "abc" 0)
      (insert-str "abc" 0)
                                        ;(insert-str "abc" 0)
      )
  )


