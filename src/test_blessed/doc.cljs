(ns test-blessed.doc
  (:require [test-blessed.tree :as t]
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
       (vec)
       
       (partition 2)
       (map (partial apply str))))

(comment
  (split-lines "abc\n\nc\n")

  (-> "abc\n\nabc\n"
      (.split  "\n")
      (js->clj)
      )

  (interpose "\n" ["abc" "bcd"])
  
  (split-lines "abc\n\nabc\n")
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
  (let [[text [offset num]] (find-one-at-idx lines idx)]
    (-> lines
        (cond-> num
          (t/delete num (inc num) second))
        (insert-lines (or offset idx) (insert-to-str (or text "") (- idx (or offset 0)) arg)))))

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

(comment
  (truncate-marker [0 5] 0 3)
  (truncate-marker [0 5] 2 5)
  (truncate-marker [0 5] 2 1)
  (truncate-marker [5 10] 3 3)
  (truncate-marker [5 10] 0 4)
  )

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
    (delete-lines tree lines))
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
