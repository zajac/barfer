(ns barfer.app
  (:require [reagent.core :as re]
            [reagent.ratom :as ra]
            [barfer.doc :as doc]
            [barfer.tree :as t]
            [barfer.caret :as c]
            [clojure.string]
            [keybind.core :as key]))


 
(def model-0 (first (-> [{:lines (->
                                (t/make-tree)
                                (t/insert-str "Hello world\n" 0)
                                (t/insert-str "bless you" 12))
                        :markup (->
                                 (t/make-tree)
                                 (t/insert {:length 5
                                            :attrs {:foreground "red"
                                                    :background "black"}} 0 5)
                                 (t/insert {:length 16} 6 16)
                                 )
                        :caret {:line 0
                                :col 0}} 0]
                      (doc/play [:insert "Hello world 22"])
                      (doc/play [:insert "\n bless you\n 333"])
                      (doc/play [:insert "hello again 44444"]))))

(defonce model (ra/atom model-0))

(defn type! [str]
  (swap! model c/type-in str))

(defn delete! []
  (swap! model c/delete-char))

(defn move-cursor! [dir]
  (swap! model
         c/move-cursor dir))

(comment
  (type! "huyasdasdsad")
  @model
  (move-cursor! :down)
  (swap! model update :markup
         (fn [markup]
           (t/insert markup {:length 5
                             :attrs {:foreground "blue"
                                     :background "purple"}}
                     0 3)))
  
  )

(defn split-by-markup [markup [text [line-offset number]]]
  (->> markup
       (map (fn [[{:keys [attrs length]} [marker-offset _]]]
              (let [rel-offset (- marker-offset line-offset)]
                {:attrs attrs
                 :text (clojure.string/trim-newline (subs text rel-offset (+ rel-offset length)))})))
       (filter (complement (comp empty? :text)))))

(defn line [markup l i]
  (js/console.log "reconciling line" i)
  [:div
   (for [{{:keys [foreground background]} :attrs
          text                            :text} (split-by-markup markup l)]
     ^{:key (hash [text foreground background])}
     [:span
      {:style {:color (or foreground "green")
               :background-color (or background "white")}}
      text])])

(do
  (key/bind! "down"
             ::down
             #(move-cursor! :down))
  (key/bind! "up"
             ::up
             #(move-cursor! :up))
  (key/bind! "left"
             ::left
             #(move-cursor! :left))
  (key/bind! "right"
             ::right
             #(move-cursor! :right))
  (key/bind! "delete"
             ::delete
             delete!)
  (key/bind! "enter"
             ::enter
             #(type! "\n"))
  )

(defn barfer [model]
  (let [lines (reaction (t/query (:lines @model) 0 40 second))
        markup (:markup @model)
        markers (t/query markup 0 10000 first)]
    [:div
     
     [:div {:style {:overflow :hidden
                    :position :relative
                    :width "3px"
                    :height "0px"}}
      [:textarea {:auto-focus true
                  :onInput #(prn "input!!!")
                  :autocorrect "off"
                  :autocapitalize "off"
                  :spellcheck false
                  :style {:position :absolute
                          :bottom  "-1em"
                          :padding "0px"
                          :width "1000px"
                          :height "1em"
                          :outline "none"}
                  :tab-index 0
                  :on-change (fn [evt]
                               (let [elt (.-target evt)
                                     val (.-value elt)]
                                 (type! (or val ""))
                                 (aset elt "value" "")
                                 true))
                  :on-focus (fn [e]
                              (js/console.log "textarea focused")
                              true)}]]
     (map-indexed
      (fn [i l]
        ^{:key i}
        [:div
         [line markers l i]])
      lines)]))

(defn main []
  (let [e (js/document.getElementById "container")]
    (re/render [barfer model] e)))


(comment
  

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

  


  

  (comment

    (line (t/query (:markup @model-ptr) 0 1000  first)
          (first (t/query (:lines @model-ptr) 3 4 second)))

    )

  (defn caret [{:keys [line col]}]
    [:box
     {:top line
      :left col
      :height 1
      :width 1
      :style {:bg "green"
              :transparent true}}
     " "])

  (defn editor [model]
    (let [lines (t/query (:lines @model) 0 40 second)
          markup (:markup @model)
          markers (t/query markup 0 10000 first)]
      [box
       {:border {:type :line}
        :style {:border {:fg "red"}}
        :height "40%" :width "40%"
        :left "center" :top "center"}
       (concat (map-indexed
                (fn [i l]
                  ^{:key i}
                  [:box
                   {:top i
                    :height 1
                    :width "100%"}
                   [line markers l]])
                lines)
               [[caret (:caret @model)]])]))

  (defonce model-ptr (rea/atom model))

  (defn -editor []
    [editor model-ptr])

  (comment
    (doc/get-line-size (:lines @model-ptr) 0)
    (wrap-cursor (:lines @model-ptr) {:line 0 :col 13})
    (restrain-cursor (:lines @model-ptr) {:line 0 :col 15})
    )





  

  (comment
    (move-cursor @model-ptr :down)
    )



  (comment
    (let [lines (:lines @model-ptr)]
      (doc/offset->pos 13 lines)
      (doc/pos->offset {:line 1 :col 3} lines)
      (ins-op "abcd" @model-ptr)))


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
    (.render screen))

  (defonce fw (fw/start {:on-jsload barfer.core/main
                         :websocket-url "ws://localhost:5309/figwheel-ws"}))

  (set! *main-cli-fn* main)
  )
