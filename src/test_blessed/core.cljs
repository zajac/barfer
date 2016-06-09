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

(defn find
  ([tree from to select] (find tree [0 0] from to select))
  ([[children data :as tree] acc from to select]
   (if (not (leaf? tree))
     (second
      (reduce (fn [[acc res] c]
                (let [sel (select (second c))
                      acc-sel (select acc)]
                  [(sum acc (second c))
                   (if (intersects? [from to] [acc-sel (+ acc-sel sel)])
                     (concat res (find c acc from to select))
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
      (find  6 700 first)
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
                        (insert {})
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
  (first (find tree idx (inc idx) first)))

(defn insert-str [text ins-offset arg]
  (let [low (subs text 0 ins-offset)
        high (subs text ins-offset)]
    (str low arg high)))

(defn play [[{:keys [lines markup] :as model} idx] [op arg]]
  (case op
    :retain [model (+ idx arg)]
    :insert (let [[text [offset num]] (or (find-one-at-idx lines (dec idx))
                                          ["" [0 0]])
                  lines (delete lines num (inc num) second)
                  [lines _]  (reduce (fn [[lines offset] line]
                                       [(insert lines line offset (count line))
                                        (+ offset (count line))])
                                     [lines idx]
                                     (split-lines (insert-str text (- idx offset) arg)))
                  [marker [marker-offset marker-num]] (find-one-at-idx markup idx)
                  markup (if marker
                           (let [new-marker (assoc marker :length (+ (:length marker) (count arg)))]
                             (-> markup
                                 (delete marker-num (inc marker-num) second)
                                 (insert new-marker marker-offset (:length new-marker))))
                           (insert markup {:length (count arg)} idx (count arg)))]
              [(-> model
                   (assoc :markup markup)
                   (assoc :lines lines)) (+ idx (count  arg))])))

(comment

  
  (let [[lines _] (reduce (fn [[lines offset] line]
                            [(insert lines line offset (count line))
                             (+ offset (count line))])
                          [(insert (make-tree) "Hello world" 0 11) 11] 
                          (split-lines (insert-str "" (- 11 0) "\n fuck you")))]
    lines)
  
  (split-lines (insert-str "Hello world" 11 "\n fuck you"))
  
  (find-one-at-idx (make-tree) 0)
  (-> [{:lines (make-tree)
          :markup (make-tree)
          } 0]
        (play [:insert "Hello world 1"])
        (play [:insert "\n fuck you 222 \n"])
        (play [:insert "hello again 44"])
        )
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
