(ns test-blessed.core
  (:require [cljs.nodejs :as nodejs]
            [reagent.core :as re]
            [reagent.ratom :as rea]
            [figwheel.client :as fw]
            [test-blessed.doc :as doc]
            [test-blessed.tree :as t]))

(defonce bl (js/require "blessed"))
(defonce rbl (js/require "react-blessed"))
(defonce screen (.screen bl #js {:autopadding true
                                 :smartcsr true
                                 :title "fuck you"}))
(defonce blessed-render (.-render rbl))

(nodejs/enable-util-print!)

(defonce qh (.key screen #js ["C-c"] #(js/process.exit 0)))

(defn box [m ch]
  (into [:box m] ch))

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
   (t/query (:lines model) 0 100000 first)))

(def model (first (-> [{:lines (->
                                (t/make-tree)
                                (t/insert-str "Hello world\n" 0)
                                (t/insert-str "fuck you" 12))
                        :markup (->
                                 (t/make-tree)
                                 (t/insert {:length 5
                                          :attrs {:foreground "red"
                                                  :background "black"}} 0 5)
                                 (t/insert {:length 14} 6 14)
                                 )
                        :caret {:line 0
                                :col 0}} 0]
                      (doc/play [:insert "Hello world 22"])
                      (doc/play [:insert "\n fuck you\n 333"])
                      (doc/play [:insert "hello again 44444"])
                      ))

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
                   (+ offset (count text))])
                [[] 0])
        first)])

(comment

  (line (t/query (:markup @model-ptr) 0 1000  first)
        (first (t/query (:lines @model-ptr) 3 4 second)))

  )

(defn editor [model]
  (let [lines (t/query (:lines @model) 0 40 second)
        markup (:markup @model)
        markers (t/query markup 0 10000 first)
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

(comment
  (doc/get-line-size (:lines @model-ptr) 0)
  (wrap-cursor (:lines @model-ptr) {:line 0 :col 13})
  (restrain-cursor (:lines @model-ptr) {:line 0 :col 15})
  )

(defn wrap-cursor [lines {:keys [col line] :as caret}]
  (let [current-line-size (doc/get-line-size lines line)
        last-line-idx (dec (second (second lines)))]
    (cond
      (and (not= line last-line-idx) (> col current-line-size) ) {:col 0 :line (inc line)}
      (and (< col 0) (not= line 0)) {:col (doc/get-line-size lines (dec line))
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
                              :col (min col (doc/get-line-size lines next-line-fixed))}
                       :up {:line (dec line)
                            :col (min col (doc/get-line-size lines (dec line)))}
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

(defn ins-op [caret-offset str {:keys [lines caret]}]
  [[:retain caret-offset]
   [:insert str]
   [:retain (- (doc/doc-len lines) caret-offset)]])

(comment
  (let [lines (:lines @model-ptr)]
    (offset->pos 13 lines)
    (pos->offset {:line 1 :col 3} lines)
    (ins-op "abcd" @model-ptr)
    )
  )

(defn play-op [model ops]
  (first (reduce doc/play [model 0] ops)))

(defn type-in [{:keys [lines caret] :as model} str]
  (let [caret-offset (doc/pos->offset caret lines)
        ops (ins-op caret-offset str model)]
    (-> model                 
        (play-op ops)
        (#(assoc % :caret (doc/offset->pos (+ caret-offset (count str) ) (:lines %)))) )))

(defn del-op [n {:keys [lines caret]}]
  (let [caret-offset (doc/pos->offset caret lines)]
    {:ops [[:retain caret-offset]
           [:delete n]
           [:retain (- (doc/doc-len lines) n caret-offset)]] 
     :caret caret}))

(defn delete-char [model]
  (let [{:keys [ops]} (del-op 1 model)]
    (-> model                 
        (play-op ops))))

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
