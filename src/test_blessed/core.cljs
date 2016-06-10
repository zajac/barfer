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


(defn box [m ch]
  (into [:box m] ch))


(comment

  (line markup {:num 0
                :offset 0
                :markup [0 1]
                :text "hello world"})
  )

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
   (let [[ins & [rest]] (insert t s offset len :fuck)]
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
                                        ;   (prn "idx" idx "acc" acc "c" c)
     (let [[low high] (split-at idx children)]
       (-> (concat low (if (or (leaf? c) (nil? c))                         
                         [(make-leaf s [len 1])]
                         (insert c s (- offset acc) len :fuck))
                   (if (or (leaf? c) (nil? c))                         
                     high
                     (rest high)))
           (split-subtree (sum data [len 1])))))))

(split-at 2 [1 2 3 4])

(comment

  (prn-lines @model-ptr)
  
  (-> (make-tree)
      (insert-str "abc" 0)
      (insert-str "abc" 0)
      ;(insert-str "abc" 0)
      )
  )

(defn intersects? [[from1 to1] [from2 to2]]
  (< (max from1 from2) (min to1 to2)))

(comment
  (intersects? [1 5] [2 3])
  (intersects? [1 5] [3 10])
  (intersects? [1 5] [5 6]))

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

(defn insert-str [tree str offset]
  (insert tree str offset (count str)))

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
                   ;(prn "INSERTING" "lines" lines "line" line "offset" offset)
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
  (let [intersecting-lines (query lines idx (+ idx arg 1) first)        
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
          (delete num (inc num) second))
        (insert-lines (or offset idx) (insert-to-str (or text "") (- idx (or offset 0)) arg)))))

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

(defn split-by-markup [markup [text [line-offset number]]]
  (->> markup
       (map (fn [[{:keys [attrs length]} [marker-offset _]]]
              (let [rel-offset (- marker-offset line-offset)]
                {:attrs attrs
                 :text (clojure.string/trim-newline (subs text rel-offset (+ rel-offset length)))})))
       (filter (complement (comp empty? :text)))))

(comment
  (split-by-markup [[{:attrs {:foreground "red"}
                      :length 3} [0 0]]
                    [{:length 1000} [3 0]]]
                   ["Hellolo" [0 0]])
)

(defn prn-lines [model]
  (reduce
   (fn [s [text _]]
     (str s text))
   ""
   (query (:lines model) 0 100000 first)))

#_(def model {:lines (->
                    (make-tree)
                    (insert-str "Hello world" 0)
                    )
            :markup (->
                     (make-tree)
                     (insert {:length 11 } 0 11))
            :caret {:line 0
                    :col 0}})

(def model (first (-> [{:lines (->
                                (make-tree)
                                (insert-str "Hello world\n" 0)
                                (insert-str "fuck you" 12))
                        :markup (->
                                 (make-tree)
                                 (insert {:length 5
                                          :attrs {:foreground "red"
                                                  :background "black"}} 0 5)
                                 (insert {:length 14} 6 14)
                                 )
                        :caret {:line 0
                                :col 0}} 0]
                      (play [:insert "Hello world 22"])
                      (play [:insert "\n fuck you\n 333"])
                      (play [:insert "hello again 44444"])
                      ))

  )


(comment

  (prn-lines @model-ptr)

  (let [state [{:lines (make-tree)
                :markup (make-tree)
                :caret {:line 0 :col 0}} 0]
        state (play state [:insert "h\n"])
        [model _] (play state [:insert "a"])
        state (play [model 0] [:retain 1])
        state (play state [:delete 1])
        ]
    state)
  
  

  (prn-lines (first (-> [{:lines (->
                                  (make-tree)
                                  (insert-str "Hello world" 0)
                                  (insert-str "fuck you" 11))
                          :markup (->
                                   (make-tree)
                                   (insert {:length 5
                                            :attrs {:foreground "red"
                                                    :background "black"}} 0 5)
                                   (insert {:length 14} 6 14)
                                   )
                          :caret {:line 0
                                  :col 0}} 0]
                        (play [:insert "666"])
                        (play [:insert "\n fuck you\n 333"])
                        (play [:insert "hello again 44444"])
                        )))

  (prn-lines (first (-> [{:lines (->
                                  (make-tree)
                                  (insert-str "Hello world\n" 0)
                                  (insert-str "fuck you" 12))
                          :markup (->
                                   (make-tree)
                                   (insert {:length 5
                                            :attrs {:foreground "red"
                                                    :background "black"}} 0 5)
                                   (insert {:length 14} 6 14)
                                   )
                          :caret {:line 0
                                  :col 0}} 0]
                        (play [:insert "Hello world 22"])
                        (play [:insert "hello again 44444"])
                        )))


  )


(defn line [markup l]
  [box {:top 0
        :left 0
        :width "100%"
        :heigh 1}
   (->> (split-by-markup markup l)
        (reduce (fn [[acc offset] {{:keys [foreground background]} :attrs
                                   text :text}]
                  [(conj acc
                         [:box
                          {:fg foreground
                           :bg background
                           :width (count text)
                           :left offset}
                          text])
                   (+ offset (count text))]) [[] 0])
        first)])

(defn editor [model]
  (let [lines (query (:lines @model) 0 40 second)
        markup (:markup @model)
        markers (query markup 0 10000 first)
        caret (:caret @model)]
    [box
     {:border {:type :line}
      :style {:border {:fg "red"}}
      :height "40%" :width "40%"
      :left "center" :top "center"}
     (concat (map-indexed
            (fn [i l]
              [:box
               {:top i 
                :height 1
                :width "100%"}
               [line markers l]]) lines)
             [[:box
                {:top (:line caret)
                 :left (:col caret)
                 :height 1
                 :width 1
                 :style {:bg "green"
                         :transparent true}}
               " "]])]))

(defonce model-ptr (rea/atom model))

(defn -editor []
  [editor model-ptr])

(defn get-line-size [lines num]
  (let [[text [offset _]] (first (query lines num (inc num) second))]
    (if (not= \newline (last text))
      (count text)
      (dec (count text)))))

(comment
  (get-line-size (:lines @model-ptr) 0)
  (wrap-cursor (:lines @model-ptr) {:line 0 :col 13})
  (restrain-cursor (:lines @model-ptr) {:line 0 :col 15})
  )

(defn wrap-cursor [lines {:keys [col line] :as caret}]
  (let [current-line-size (get-line-size lines line)
        last-line-idx (dec (second (second lines)))]
    (cond
      (and (not= line last-line-idx) (> col current-line-size) ) {:col 0 :line (inc line)}
      (and (< col 0) (not= line 0)) {:col (get-line-size lines (dec line))
                                     :line  (dec line)}
      :default {:col (max 0 (min current-line-size col))
                :line (max line 0)})))

(defn move-cursor [{:keys [lines] :as model} dir]
  (update model :caret
          (fn [{:keys [line col] :as caret}]
            (let [last-line-idx (dec (second (second lines)))
                  next-line-fixed (min (inc line) last-line-idx)]
              (->> 
                     (case dir
                       :down {:line next-line-fixed
                              :col (min col (get-line-size lines next-line-fixed))}
                       :up {:line (dec line)
                            :col (min col (get-line-size lines (dec line)))}
                       :left (update caret :col dec)
                       :right (update caret :col inc)
                       caret)
                     (wrap-cursor lines))))))




(defn move-cursor! [dir]
  (swap! model-ptr
         move-cursor dir))

(comment
  (move-cursor @model-ptr :down)
  )

(defn pos->offset [{:keys [line col]} lines]
  (when-let [[_ [line-offset _]]
             (first (query lines line (inc line) second))]
    (+ line-offset col)))

(defn doc-len [lines]
  (first (second lines)))

(defn lines-count [lines]
  (second (second lines)))

(defn offset->pos [offset lines]
  (when-let [[_ [line-offset num]] (first (query lines offset (inc offset) first))]
    {:line num
     :col (- offset line-offset)}))

(defn ins-op [caret-offset str {:keys [lines caret]}]
  [[:retain caret-offset]
   [:insert str]
   [:retain (- (doc-len lines) caret-offset)]])

(comment
  (let [lines (:lines @model-ptr)]
    (offset->pos 13 lines)
    (pos->offset {:line 1 :col 3} lines)
    (ins-op "abcd" @model-ptr)
    )
  )

(defn play-op [model ops]
  (first (reduce play [model 0] ops)))

(defn type-in [{:keys [lines caret] :as model} str]
  (let [caret-offset (pos->offset caret lines)
        ops (ins-op caret-offset str model)]
    (-> model                 
        (play-op ops)
        (#(assoc % :caret (offset->pos (+ caret-offset (count str) ) (:lines %)))) )))

(defn del-op [n {:keys [lines caret]}]
  (let [caret-offset (pos->offset caret lines)]
    {:ops [[:retain caret-offset]
           [:delete n]
           [:retain (- (doc-len lines) n caret-offset)]] 
     :caret caret}))

(defn delete-char [model]
  (let [{:keys [ops]} (del-op 1 model)]
    (-> model                 
        (play-op ops))))

(comment

  (ins-op "a" @model-ptr)
  
  (type-in @model-ptr "a"))

(defn type! [str]
  (swap! model-ptr type-in str))

(defn delete! []
  (swap! model-ptr delete-char))

(defn keys-handler [_ jskey]
  (let [k (js->clj jskey)
        n (k "name")]
    
    (when (#{"down" "up" "left" "right"} n) (move-cursor! (keyword n)))
    (when (#{"a"} n) (type! n))
    (when (#{"delete"} n) (delete!))
    (when (#{"enter"} n) (type! "\n"))))



(defonce keys-handler-token (atom nil))
 
(defn main []
  (prn "load1")   
  (.clearRegion screen 0 (.-width screen) 0 (.-height screen))
  (blessed-render (.createElement js/React (re/reactify-component -editor)) screen)
  (when (not @keys-handler-token)
    (.on screen "keypress" keys-handler))
  (reset! keys-handler-token 1)
  (.render screen)
  ) 

(defonce fw (fw/start {:on-jsload test-blessed.core/main
                       :websocket-url "ws://localhost:5309/figwheel-ws"}))

(set! *main-cli-fn* main)
