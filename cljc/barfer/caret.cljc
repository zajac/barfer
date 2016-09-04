(ns barfer.caret
  (:require [barfer.doc :as doc]))

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

(defn ins-op [caret-offset str {:keys [lines caret]}]
  [[:retain caret-offset]
   [:insert str]
   [:retain (- (doc/doc-len lines) caret-offset)]])

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


