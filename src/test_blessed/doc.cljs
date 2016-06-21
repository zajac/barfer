(ns barfer.doc
  (:require [barfer.tree :as t]
            [clojure.string]))

(defn delete-from-str [text from count]
  (str (subs text 0 from) (subs text (+ from count))))

(defn find-one-at-idx [tree idx]
  (first (t/query tree idx (inc idx) first)))

(defn insert-to-str [text ins-offset arg]
  (let [low (subs text 0 ins-offset)
        high (subs text ins-offset)]
    (str low arg high)))

(defn split-lines [s]
  (->> (.split s "\n")
       (interpose "\n")
       (partition 2 2 [""])
       (map (partial apply str))
       (filter (complement empty?))))

(comment
  (split-lines "abc\n\nc\n")
  (split-lines "abc\n")
  (split-lines "abc\n\nabc")
  )

(defn insert-lines [lines idx text]
  (first (reduce (fn [[lines offset] line]
                   [(t/insert lines line offset (count line))
                    (+ offset (count line))])
                 [lines idx]
                 (split-lines text))))

(defn delete-lines [lines lines-to-delete length-fun]
  (let [[_ [start-idx _]] (first lines-to-delete)
        [last-line [last-line-idx _]] (last lines-to-delete)
        end-idx (+ last-line-idx (length-fun last-line))]
    (t/delete lines start-idx end-idx first)))

(defn delete-from-lines-tree [lines idx arg]
  (let [intersecting-lines (t/query lines idx (+ idx arg 1) first)        
        lines (delete-lines lines intersecting-lines count)]
      (if (= 1 (count intersecting-lines))
        (let [[text [offset _]] (first intersecting-lines)
              relative-offset (- idx offset)]
          (insert-lines lines offset (delete-from-str text relative-offset arg)))
        (let [[text-first [offset-first _]] (first intersecting-lines)
              [text-last [offset-last _]] (last intersecting-lines)
              first-rel-offset (- idx offset-first)
              last-rel-offset (- (+ arg idx) offset-last)
              first-line-trimmed (delete-from-str text-first first-rel-offset 10000)
              last-line-trimmed (delete-from-str text-last 0 last-rel-offset)]
          (insert-lines lines offset-first (str first-line-trimmed
                                                last-line-trimmed))))))

(defn insert-to-lines-tree [lines idx arg]
  (let [affected-lines (t/query lines (dec idx) (inc idx) first)
        joined-text (reduce (fn [s [text _]] (str s text)) "" affected-lines)
        [_ [offset num]] (first affected-lines)
        lines-to-insert (insert-to-str joined-text (- idx (or offset 0)) arg)]
    (-> lines
        (cond-> num
          (t/delete num (+ num (count affected-lines)) second))
        (insert-lines (or offset idx) lines-to-insert))))

(defn insert-to-markup-tree [markup idx count]
  (let [[marker [marker-offset marker-num]] (find-one-at-idx markup idx)]
    (if marker
      (let [new-marker (assoc marker :length (+ (:length marker) count))]
        (-> markup
            (t/delete marker-num (inc marker-num) second)
            (t/insert new-marker marker-offset (:length new-marker))))
      (t/insert markup {:length count} idx count))))



(defn intersection [[start1 end1] [start2 end2]]
  [(max start1 start2) (min end1 end2)])

(defn truncate-marker [[offset len] idx count]
  (let [marker [offset (+ offset len)]
        deletion [idx (+ idx count)]
        [i-start i-end] (intersection marker deletion)
        new-len (if (t/intersects? marker deletion)
                  (- len (- i-end i-start))
                  len)]
    (if (< offset idx)
      [offset new-len]
      (if (< offset (+ idx count))
        [idx new-len]
        [(- offset count) new-len]))))

(defn cut-marker [[offset len] idx count]
  (->> (cond
        (not (t/intersects? [offset (+ offset len)] [idx (+ idx count)])) [[offset len]]
        (<= offset idx (+ offset len) (+ idx count)) [[offset (- idx offset)]]
        (<= offset idx (+ idx count) (+ offset len)) [[offset (- idx offset)] [(+ idx count) (- (+ offset len) (+ idx count))]]
        (<= idx offset (+ offset len) (+ idx count)) [[idx (- (+ idx count) (+ len offset))]]
        (<= idx offset (+ idx count) (+ offset len)) [[(+ idx count) (- (+ offset len) (+ idx count))]]
        :default '()
        )
       (filter (comp (partial < 0) second))))

(comment
  (cut-marker [0 3] 1 1)
  (cut-marker [0 3] 1 100)
  (cut-marker [0 3] 0 1)
  (cut-marker [0 3] -1 2)
  (cut-marker [0 1] 5 6)
  (cut-marker [1 1] 0 2)
  )

(defn cut-markers [offset len intersecting]
  (->> intersecting
       (mapcat (fn [[marker [marker-offset _]]]
                 (map (fn [x]
                        [marker x])
                      (cut-marker [marker-offset (:length marker)]
                                  offset len))))
       (map (fn [[marker [offset len]]]
              [(assoc marker :length len) offset]))
       (filter (comp (partial < 0) :length first))))

(comment
  (truncate-markers 1 2
                    [[{:id 0 :length 2} [0 nil]]
                     [{:id 1 :length 3} [2 nil]]])
  
  (truncate-markers 0 2
                    [[{:id 0 :length 1} [0 nil]]
                     [{:id 1 :length 1} [1 nil]]])

  
  )

(defn add-marker [markup m offset]
  (let [intersecting (t/query markup offset (+ offset (:length m)) first)]
    (reduce (fn [markup [m offset]]
              (t/insert markup m offset (:length m)))
            (delete-lines markup intersecting :length)
            (->> intersecting
                 (cut-markers offset (:length m))
                 (concat [[m offset]])
                 (sort-by second)))))

(comment
  (-> (t/make-tree)
      (add-marker {:length 1} 0)
      (add-marker {:length 1} 1) 

      (add-marker {:length 2} 0) ; => [0 2]
      (add-marker {:length 3} 1) ; => [0 1] [1 4]
      (add-marker {:length 2} 1) ; => [0 1] [1 3] [3 4]
      (add-marker {:length 4} 0)
      (t/query 0 1000 first))
  
  (truncate-marker [0 5] 0 3)
  (truncate-marker [0 5] 2 5)
  (truncate-marker [0 5] 2 1)
  (truncate-marker [5 10] 3 3)
  (cut-marker [0 1] 0 4)

  
  (truncate-marker [5 10] 0 4))
  

(defn delete-from-markup-tree [markup idx cnt]
  (let [intersecting-markups (t/query markup idx (+ idx cnt) first)
        markup (delete-lines markup intersecting-markups :length)]
    (if (= 1 (count intersecting-markups))
      (let [[marker [offset _]] (first intersecting-markups)
            [offset len] (truncate-marker [offset (:length marker)] idx cnt)]        
        (t/insert markup (assoc marker :length len) offset len))      
      (let [[marker-first [offset-first _]] (first intersecting-markups)
            [marker-last [offset-last _]] (last intersecting-markups)
            [offset-first len-first] (truncate-marker [offset-first (:length marker-first)] idx cnt)
            [offset-last len-last] (truncate-marker [offset-last (:length marker-last)] idx cnt)]
        (-> markup
            (t/insert (assoc marker-first :length len-first) offset-first len-first)
            (t/insert (assoc marker-last :length len-last) offset-last len-last))))))

(defn play [[{:keys [lines markup] :as model} idx] [op arg]]
  (case op
    :retain [model (+ idx arg)]
    :insert [(merge model
                    {:markup (insert-to-markup-tree markup idx (count arg))
                     :lines (insert-to-lines-tree lines idx arg)})
              (+ idx (count  arg))]
    :delete  [ (merge model
                      {:lines (delete-from-lines-tree lines idx arg)
                       :markup (delete-from-markup-tree markup idx arg)})
              idx]))

(comment
  (let [tree (-> (t/make-tree)
                 (t/insert-str "hello" 0)
                 (t/insert-str "hello2" 0)
                 (t/insert-str "hello34" 0))
        lines (t/query tree 8 14 first)]
    (delete-lines tree lines count))
  )


(defn get-line-size [lines num]
  (let [[text [offset _]] (first (t/query lines num (inc num) second))]
    (if (not= \newline (last text))
      (count text)
      (dec (count text)))))

(defn doc-len [lines]
  (first (second lines)))

(defn lines-count [lines]
  (second (second lines)))

(defn offset->pos [offset lines]
  (when-let [[_ [line-offset num]] (first (t/query lines offset (inc offset) first))]
    {:line num
     :col (- offset line-offset)}))

(defn pos->offset [{:keys [line col]} lines]
  (when-let [[_ [line-offset _]]
             (first (t/query lines line (inc line) second))]
    (+ line-offset col)))
