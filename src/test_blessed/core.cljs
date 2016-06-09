(ns test-blessed.core
  (:require [cljs.nodejs :as nodejs]
            [reagent.core :as re]
            [reagent.ratom :as rea]
            [figwheel.client :as fw]))

(defonce bl (js/require "blessed"))
(defonce rbl (js/require "react-blessed"))
(defonce screen (.screen bl #js {:autopadding true
                                 :smartcsr true
                                 :title "fuck you"}))
(defonce blessed-render (.-render rbl))

(nodejs/enable-util-print!)

(def pos (rea/atom 0))

(defn scroll! []
  (swap! pos inc))

(defonce kh (.key screen #js ["p"] #(scroll!)))
(defonce qh (.key screen #js ["C-c"] #(js/process.exit 0)))

(def markup {0 {:startPos {:line 0
                           :col 0}
                :endPos {:line 0
                         :col 5}
                :attrs {:foreground "red"
                        :background "black"}}
             1 {
                :startPos {:line 0
                           :col 6}
                :endPos {:line 0
                         :col 12}
                :attrs {:foreground "yellow"
                        :background "black"}}})

(def ds [{:num 0
          :offset 0
          :markup [0 1]
          :text "hello world"}
         {:offset 12
          :num 1
          :markup []
          :text "fuck you"}])

(defn box [m ch]
  (into [:box m] ch))

(defn split-by-markup [markup-db {:keys [text num markup]}]
  (let [markups (conj (into [nil] (map markup-db markup)) nil)]
    (->> markups
     (partition 2 1)
     (mapcat (fn [[p n]]
               (let [{{n-line :line
                       n-start :col} :startPos}  n
                     {{p-start-line :line
                       p-start :col} :startPos
                      {p-end-line :line
                       p-end :col} :endPos} p
                     
                     n-start (if (< n-line num)
                               0
                               n-start)
                     
                     p-start (if (< p-start-line num)
                               0
                               p-start) 
                     p-end (if (> p-end-line num)
                             (count text)
                             p-end)]
                 (cond
                   (and (nil? p) (nil? n)) [[text nil]]
                   (nil? p) [[(subs text 0 n-start) nil]]
                   (nil? n) [[(subs text p-start p-end) p]
                             [(subs text p-end (count text)) nil]]
                   :default [[(subs text p-start p-end) p]
                             [(subs text p-end n-start) nil]]))))
     (filter (comp not-empty first)))))

(comment
  (split-by-markup {0 {:attrs {:foreground "red"}
                       :startPos {:line 0 :col 5}
                       :endPos {:line 0 :col 8}}}
                   {:num 0 :text "fuck you" :markup [0]})

  (split-by-markup {0 {:attrs {:foreground "red"}
                       :startPos {:line -1 :col 5}
                       :endPos {:line 0 :col 5}}}
                   {:num 0 :text "fuck you" :markup [0]})

  (split-by-markup {0 {:attrs {:foreground "red"}
                       :startPos {:line 0 :col 5}
                       :endPos {:line 1 :col 5}}}
                   {:num 0 :text "fuck you" :markup [0]})

  (split-by-markup {0 {:attrs {:foreground "red"}
                       :startPos {:line 0 :col 5}
                       :endPos {:line 0 :col 8}}
                    1 {:attrs {:foreground "yellow"}
                       :startPos {:line 0 :col 12}
                       :endPos {:line 1 :col 5}}}
                   {:num 0 :text "fuck you the world" :markup [0 1]})
)

(defn line [markup-db l]
  [box {:top 0
        :left 0
        :width "100%"
        :heigh 1}
   (->> (split-by-markup markup-db l)
        (reduce (fn [[acc offset] [str {{:keys [foreground background]} :attrs}]]
                  [(conj acc
                         [:box
                          {:fg foreground
                           :bg background
                           :width (count str)
                           :left offset}
                          str])
                   (+ offset (count str))]) [[] 0])
        first)])

(comment

  (line markup {:num 0
                :offset 0
                :markup [0 1]
                :text "hello world"})
  )

(defn editor [markup-db model]
  [box
   {:border {:type :line}
    :style {:border {:fg "red"}}
    :height "100%" :width "100%" :left 100 :top 40}
   (map-indexed
    (fn [i l]
      [:box
       {:top i 
        :height 1
        :width "100%"}
       [line @markup-db l]]) @model)])

(defn -editor []
  [editor (atom markup) (rea/atom ds)])

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
        [(list [left (sub-sum left)] [right (sub-sum right)]) data])
      [children data])))

(defn insert [[children data] s offset len]
  (let [idx (first
             (reduce (fn [[idx acc] [_ [off _]]]
                       (if (< offset (+ off acc))
                         (reduced [idx acc])
                         [(inc idx) (+ off acc)]))
                     [0 0]
                     children))
        [low high] (split-at idx children)]
    (-> (concat low [(make-leaf s [len 1])] high)
        (split-subtree (sum data [len 1])))))

(defn intersects? [[from1 to1] [from2 to2]]
  (< (max from1 from2) (min to1 to2)))

(comment
  (intersects? [1 5] [2 3])
  (intersects? [1 5] [3 10])
  (intersects? [1 5] [5 6]))

(defn query
  ([tree from to select] (query tree [0 0] from to select))
  ([[children data :as tree] acc from to select]
   (prn acc tree from to)
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

(defn contains-range? [[from1 to1] [from2 to2]]
  (and  (<= from1 from2) (<= to2 to1)))

(comment
  (contains-range? [1 5] [2 3])
  (contains-range? [1 5] [3 5])
  
  )

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

(comment

  
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

(defn insert-str [tree str offset]
  (insert tree str offset (count str)))

(def model {:markup (-> (make-tree)
                        (insert {:attrs {:foreground "red"
                                         :background "black"}}
                                6 5)
                        
                        (insert {:attrs {:foreground "yellow"
                                         :background "black"}}
                                12 4))
            :lines (-> (make-tree)
                       (insert-str "hello world\n" 0)
                       (insert-str "fuck you\n" 12))
            })

(defn split-lines [s]
  (->> s
       (clojure.string/split-lines)
       (interpose "\n")
       (vec)
       (#(conj % (if (= (last s) \newline) "\n" nil)))       
       (partition 2)
       (map (partial apply str))))

(comment
  (split-lines "abc\nc\n")
  (interpose "\n" [1 2 3])
  )

(defn find-one-at-idx [tree idx]
  (first (query tree idx (inc idx) first)))

(defn insert-to-str [text ins-offset arg]
  (let [low (subs text 0 ins-offset)
        high (subs text ins-offset)]
    (str low arg high)))

(defn insert-lines [lines idx text]
  (first (reduce (fn [[lines offset] line]
                   [(insert lines line offset (count line))
                    (+ offset (count line))])
                 [lines idx]
                 (split-lines text))))


(defn delete-lines [lines lines-to-delete length-fun]
  (let [[_ [start-idx _]] (first lines-to-delete)
        [last-line [last-line-idx _]] (last lines-to-delete)
        end-idx (+ last-line-idx (length-fun last-line))]
    (delete lines start-idx end-idx first)))


(comment
  (let [tree (-> (make-tree)
                 (insert-str "hello" 0)
                 (insert-str "hello2" 0)
                 (insert-str "hello34" 0))
        lines (query tree 8 14 first)]
    (delete-lines tree lines))
  )

(defn delete-from-str [text from count]
  (str (subs text 0 from) (subs text (+ from count))))

(defn delete-from-lines-tree [lines idx arg]
  (let [intersecting-lines (query lines idx (+ idx arg) first)
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
  (let [[text [offset num]] (or (find-one-at-idx lines (dec idx))
                                ["" [0 0]])]
    (-> lines
        (delete num (inc num) second)
        (insert-lines idx (insert-to-str text (- idx offset) arg)))))

(defn insert-to-markup-tree [markup idx count]
  (let [[marker [marker-offset marker-num]] (find-one-at-idx markup idx)]
    (if marker
      (let [new-marker (assoc marker :length (+ (:length marker) count))]
        (-> markup
            (delete marker-num (inc marker-num) second)
            (insert new-marker marker-offset (:length new-marker))))
      (insert markup {:length count} idx count))))

(defn intersection [[start1 end1] [start2 end2]]
  [(max start1 start2) (min end1 end2)])

(defn truncate-marker [[offset len] idx count]
  (let [marker [offset (+ offset len)]
        deletion [idx (+ idx count)]
        [i-start i-end] (intersection marker deletion)
        new-len (if (intersects? marker deletion)
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
  (let [intersecting-markups (query markup idx (+ idx cnt) first)
        markup (delete-lines markup intersecting-markups :length)]
    (if (= 1 (count intersecting-markups))
      (let [[marker [offset _]] (first intersecting-markups)
            [offset len] (truncate-marker [offset (:length marker)] idx cnt)]        
        (insert markup (assoc marker :length len) offset len))      
      (let [[marker-first [offset-first _]] (first intersecting-markups)
            [marker-last [offset-last _]] (last intersecting-markups)
            [offset-first len-first] (truncate-marker [offset-first (:length marker-first)] idx cnt)
            [offset-last len-last] (truncate-marker [offset-last (:length marker-last)] idx cnt)]
        (-> markup
            (insert (assoc marker-first :length len-first) offset-first len-first)
            (insert (assoc marker-last :length len-last) offset-last len-last))))))

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

  
  (let [[lines _] (reduce (fn [[lines offset] line]
                            [(insert lines line offset (count line))
                             (+ offset (count line))])
                          [(insert (make-tree) "Hello world" 0 11) 11] 
                          (split-lines (insert-str "" (- 11 0) "\n fuck you")))]
    lines)
  
  (split-lines (insert-str "Hello world" 11 "\n fuck you"))
  
  (find-one-at-idx (make-tree) 0)
  (def hello-model (first (-> [{:lines (make-tree)
                                :markup (make-tree)
                                } 0]
                              (play [:insert "Hello world 22"])
                              (play [:insert "\n fuck you\n 333"])
                              (play [:insert "hello again 44444"])
                              )))
  
  
  (delete-lines (hello-model :markup) (query (hello-model :markup) 4 (+ 4 7) first) :length)
  
  (-> [hello-model 0]
      (play [:retain 4])
      (play [:delete 7])
      (play [:delete 15]))
  )

(clojure.string/split-lines "abc")

(str "abc" nil)

(defn main []
  (prn "load")   
  (.clearRegion screen 0 (.-width screen) 0 (.-height screen))
  (blessed-render (.createElement js/React (re/reactify-component -editor)) screen)
  )

(fw/start {:on-jsload main
           :websocket-url "ws://localhost:5309/figwheel-ws"})

(set! *main-cli-fn* main)
