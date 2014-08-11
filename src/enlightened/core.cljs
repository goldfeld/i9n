(ns enlightened.core
  (:require [cljs.nodejs :as node]))

(def get-blessed (memoize #(node/require "blessed")))
(def get-screen (memoize #(.screen (get-blessed))))

(defn render []
  (.render (get-screen)))

(def ^:private *active* (atom nil))

(defn bind-global-keys
  ([key-arg fn]
     (.key (get-screen) (clj->js key-arg) fn))
  ([key-arg fn & args]
     (let [screen (get-screen)]
       (.key screen (clj->js key-arg) fn)
       (doseq [[k f] (partition 2 args)]
         (.key screen (clj->js k) f)))))

(defn switch-active-view [new-active]
  (let [active (deref *active*)]
    (reset! *active* new-active)
    (when active (.setBack active))
    (.setFront new-active)
    (render)))
    
(defn ^:private create-widget
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
  (if (seq items)
    (.setItems widget (clj->js items))
    (throw "Can't set widget's items, items not seqable.")))

(defn set-title [widget title]
  (if (seq title)
    (.prepend widget (create-text {:left 2 :content title}))
    (throw "Can't set widget's title, title not seqable.")))

(defn set-key [widget key-or-keys action]
  (.key widget (clj->js key-or-keys) action))

(defn set-key-once [widget key-or-keys action]
  (.onceKey widget (clj->js key-or-keys) action))

(defn set-keys
  [widget & keymaps]
  (doseq [kmap (partition 2 keymaps)]
    (let [[key fn] kmap
          ->js #(if (vector? %) clj->js identity)]
      (.key widget (->js key) fn))))
