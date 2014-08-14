(ns enlightened.core
  (:require [cljs.nodejs :as node]
            [cljs.core.async.impl.protocols :refer [Channel]]))

(def get-blessed (memoize #(node/require "blessed")))
(def get-screen (memoize #(.screen (get-blessed))))

(defn render []
  (.render (get-screen)))

(defn render-deferred []
  (js/setTimeout render 0))

(def ^:private *active* (atom nil))

(defn bind-global-keys
  ([key-arg fn]
     (.key (get-screen) (clj->js key-arg) fn))
  ([key-arg fn & args]
     (let [screen (get-screen)]
       (.key screen (clj->js key-arg) fn)
       (doseq [[k f] (partition 2 args)]
         (.key screen (clj->js k) f)))))

(defn channel? [x]
  (satisfies? Channel x))

(defn widget? [x]
  (.hasOwnProperty x "screen"))

(defn switch-active-view
  ([new-active main-widget]
     (.focus main-widget)
     (switch-active-view new-active)
     main-widget)
  ([new-active]
     (let [active (deref *active*)]
       (reset! *active* new-active)
       (when active (.setBack active))
       (.setFront new-active)
       (render)
       new-active)))
    
(defn create-widget
  [widget-fn mods props overrides]
  ;(let [mod->fn (comp resolve (partial symbol "framework") name)
        ;modify (apply comp (map mod->fn mods))]
  (let [modify (apply comp mods)
        override #(apply assoc % overrides)]
    (widget-fn (-> props modify override))))

(defn create-text [props]
  (.text (get-blessed) (clj->js props)))

(defn create-form
  ([props] (.form (get-blessed) (clj->js props)))
  ([mods props] (create-form [] mods props))
  ([overrides mods props] (create-widget create-form mods props overrides)))

(defn create-box
  ([props] (.box (get-blessed) (clj->js props)))
  ([mods props] (create-box [] mods props))
  ([overrides mods props] (create-widget create-box mods props overrides)))

(defn create-list
  ([props] (.list (get-blessed) (clj->js props)))
  ([mods props] (create-list [] mods props))
  ([overrides mods props] (create-widget create-list mods props overrides)))

(defn set-items [widget items]
  (.setItems widget (clj->js items))
  items)

(defn set-title [widget title]
  (if (seq title)
    (let [title-widget (create-text {:left 2 :content title})]
      (.prepend widget title-widget)
      title-widget)
    (throw "Can't set widget's title, title not seqable.")))

(defn set-key [widget key-or-keys action]
  (.key widget (clj->js key-or-keys) action))

(defn unset-key [widget key-or-keys action]
  (.unkey widget (clj->js key-or-keys) action))

(defn set-key-once [widget key-or-keys action]
  (.onceKey widget (clj->js key-or-keys) action))

(defn ->js [key-or-keys]
  (if (vector? key-or-keys) (clj->js key-or-keys) key-or-keys))

(defn set-keys [widget & keymaps]
  (doseq [[key fn] (partition 2 keymaps)]
    (.key widget (->js key) fn)))

(defn unset-keys [widget & keymaps]
  (doseq [[key fn] (partition 2 keymaps)]
    (.unkey widget (->js key) fn)))
