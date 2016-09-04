(set-env!
 :resource-paths #{"cljs" "cljc" "html"}
 :dependencies '[[adzerk/boot-cljs            "1.7.228-1"      :scope "test"]
                 [adzerk/boot-cljs-repl       "0.3.2"          :scope "test"]
                 [adzerk/boot-reload          "0.4.12"         :scope "test"]
                 [adzerk/boot-test            "1.1.2"          :scope "test"]
                 [pandeiro/boot-http          "0.7.3"          :scope "test"]
                 [crisptrutski/boot-cljs-test "0.2.2-SNAPSHOT" :scope "test"]
                 [org.clojure/clojure         "1.9.0-alpha10"]
                 [org.clojure/clojurescript   "1.9.225"]
                 [org.clojure/core.async "0.2.385"]
                 [reagent "0.6.0-rc"]
                 [com.cemerick/piggieback     "0.2.1"          :scope "test"]
                 [weasel                      "0.7.0"          :scope "test"]
                 [org.clojure/tools.nrepl     "0.2.12"         :scope "test"]
                 [org.clojure/tools.logging "0.3.1"]
                 [keybind "2.0.0"]])

(require
 '[adzerk.boot-cljs      :refer [cljs]]
 '[adzerk.boot-cljs-repl :refer [cljs-repl start-repl]]
 '[adzerk.boot-reload    :refer [reload]]
 '[adzerk.boot-test      :refer :all]
 '[crisptrutski.boot-cljs-test  :refer [exit! test-cljs]]
 '[pandeiro.boot-http    :refer [serve]]
 '[boot.pod :as pod]
 '[boot.core :as core]
 '[boot.repl]
 '[clojure.spec.test :as st])

(swap! boot.repl/*default-dependencies*
       concat '[[cider/cider-nrepl "0.13.0"]])

(swap! boot.repl/*default-middleware*
       conj 'cider.nrepl/cider-middleware)


(deftask dev []
  (comp (serve :dir "target")
        (watch)
        (speak)
        (reload :on-jsload 'barfer.app/main)
        (cljs-repl)
        (cljs :source-map true
              :optimizations :none)
        (target :dir #{"target"})))
