(ns enlightened.navigation
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as a]
            [enlightened.core :as core :refer [channel?]]
            [enlightened.widgets :as widgets]))

(declare create-pane)

(defn into-hierarchy [hierarchy nav-items]
  (reduce (fn [m x] (assoc m (keyword (first x)) (rest x)))
          hierarchy nav-items))

(defn create-hierarchy [root-item nav-items]
  (into-hierarchy {:root root-item} nav-items))

(defn toggle-back-binds [widget toggle listener]
  (if toggle
    (doto widget (.onceKey "h" listener) (.onceKey "left" listener))
    (doto widget (.unkey "h" listener) (.unkey "left" listener))))

(defn clean-widget-hooks [widget-hooks]
  (doto widget-hooks
    (dissoc :back)))

(defn async-pane [chan go-next view pos hierarchy]
  (go (let [title (a/<! chan)]
        (a/reduce (fn [opts opt]
                    (.detach (first (.-children view)))
                    (let [items (into opts opt)]
                      (go-next [title items] pos hierarchy chan)
                      items))
                  [] (a/partition 2 chan)))))

(defn create-pane
  ([current hierarchy view]
     (create-pane current hierarchy view identity {}))
  ([current hierarchy view dispatch]
     (create-pane current hierarchy view dispatch {}))
  ([current hierarchy view dispatch widget-hooks]
     (create-pane current hierarchy view dispatch widget-hooks nil))
  ([[title options :as current] hierarchy view dispatch widget-hooks chan]
     (let [hooks (clean-widget-hooks widget-hooks)
           items (vec (take-nth 2 options))
           go-nav (fn [next back-hndlr hrchy chan hks]
                    (when chan (a/close! chan))
                    (toggle-back-binds view false back-hndlr)
                    (.detach (first (.-children view)))
                    (create-pane next hrchy view dispatch hks chan))
           back (if-let [parent (get-in hierarchy [:links title])]
                  #(go-nav (:nav-item parent) (js* "this") hierarchy nil
                           (assoc hooks :back (fn [vw]
                                                (.select vw (:pos parent)))))
                  (constantly nil))
           go-next (fn [next pos hrchy chan]
                     (go-nav next back (assoc-in hrchy [:links (first next)]
                                                 {:nav-item current :pos pos})
                             chan hooks))
           go-key (fn [k pos] (if-let [next (get hierarchy k)]
                                (go-next next pos hierarchy nil)))]
       (doto view
         (toggle-back-binds true back)
         (core/set-items items)
         (core/set-title title)
         (.removeAllListeners "select"))
       (.on
        view "select"
        (fn [_ i]
          (let [action (nth options (inc (* 2 i)))]
            (condp apply [action]
              keyword? (go-key action i)
              fn?
              (let [res (action)]
                (condp apply [res]
                  vector? (go-next (-> res first rest) i
                                   (into-hierarchy hierarchy res) nil)
                  keyword? (go-key res i)
                  string? (do (core/set-items
                               view (update-in items [i] #(str % ": " res)))
                              (core/render-deferred))
                  channel? (async-pane res go-next view i hierarchy)
                  (if (.hasOwnProperty res "screen") (.detach view) res)))
              channel? (async-pane action go-next view i hierarchy)
              (dispatch action)))))
       (doseq [[handle hook] hooks] (hook view))
       (core/render)
       view)))

(defn navigation
  ([nav-items action-dispatch widget-hooks]
     (navigation nav-items action-dispatch widget-hooks (widgets/create :list)))
  ([nav-items action-dispatch widget-hooks list]
     (let [{navs :xs
            root :x} (group-by #(if (= 2 (count %)) :x :xs) nav-items)]
       (create-pane (first root)
                    (create-hierarchy root navs)
                    list action-dispatch widget-hooks))))

(defn navigation-view [nav-items action-dispatch widget-hooks]
  (navigation nav-items action-dispatch widget-hooks (widgets/list-view)))
