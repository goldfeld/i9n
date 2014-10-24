(ns i9n.node.os
  (:refer-clojure :exclude [list])
  (:require [cljs.nodejs :as node]
            [cljs.core.async :as a]
            [claude.process :as proc]
            [i9n.core :as core]
            [i9n.properties :as p]))

(declare create-args set-args)

(def get-blessed (memoize #(node/require "blessed")))
(def get-screen (memoize #(.screen (get-blessed))))

(defn render []
  (.render (get-screen)))

(defn render-deferred []
  (js/setTimeout render 0))

(def exit proc/exit)

(defn bind-global-keys
  ([key-arg fn]
     (.key (get-screen) (clj->js key-arg) fn))
  ([key-arg fn & args]
     (let [screen (get-screen)]
       (.key screen (clj->js key-arg) fn)
       (doseq [[k f] (partition 2 args)]
         (.key screen (clj->js k) f)))))

(def ^:private *active* (atom nil))

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

(defn copy [text]
  (let [chan (a/chan)
        clipboard (.. (node/require "copy-paste") noConflict silent)]
    (.copy clipboard text (fn [err res] (a/put! chan (or err res))))
    chan))

(defn paste []
  (let [chan (a/chan)
        clipboard (.. (node/require "copy-paste") noConflict silent)]
    (.paste clipboard (fn [err res] (a/put! chan res)))
    chan))

(defn types []
  [:list :text :preview :row :wrapper :commandline])

(defn create
  "Creates a widget. The overrides parameters are key-value pairs of a
  property and the desired override value.
  (See i9n.os.ui/types for valid type values.)"
  [type & overrides]
  (let [[create-fn mods props] (create-args type)]
    (create-fn overrides mods props)))

(defn set-content
  "Sets the content of a widget. Any content parameters will be
  concat'ed together to make up a list of the lines of the widget's
  body content.
  (See i9n.os.ui/types for valid type values.)"
  ([widget type title content1 content2 & contents]
     (set-content widget type title (apply concat content1 content2 contents)))
  ([widget type title content]
     (let [set-fn (type set-args)]
       (set-fn widget content)
       (set-title widget title))))

(defn alert
  ([content] (alert [] content))
  ([overrides content] (alert 2000 overrides content))
  ([timeout overrides content]
     (js/setTimeout #(.log js/console content) 100)))

(defn list
  ([title content on-select]
     (let [lst (list title content)]
       (.on lst "select" #(on-select %2 content))
       lst))
  ([title content]
     (let [lst (create :list)]
       (set-content lst :list title content)
       lst)))

(defn list-view
  ([title content on-select]
     (let [widget (list-view title content)]
       (.on widget "select" #(on-select %2 content))
       widget))
  ([title content]
     (doto (list-view)
       (set-content :list title content)))
  ([]
     (let [view (create :wrapper :height "75%")
           widget (create :list)]
       (.append view widget)
       (switch-active-view view widget)
       widget)))

(defn text
  ([title txt]
     (let [widget (text txt)]
       (set-title widget title)
       widget))
  ([txt]
     (let [widget (text)]
       (.setContent widget txt)
       widget))
  ([] (create :text)))

(defn text-view [title text]
  (let [view (create :wrapper :height "75%")
        widget (create :text)]
    (.append view widget)
    (.setContent widget text)
    (set-title widget title)
    (switch-active-view view widget)
    widget))

(defn create-text-viewer!
  ([pane text]
     (let [viewer (create-text-viewer! pane)]
       (.setContent viewer text)
       viewer))
  ([pane]
     (let [viewer (text)]
       (set-items pane [])
       (.append pane viewer)
       (.focus viewer)
       viewer)))

(defn create-args [type]
  (case type
    :list [create-list
           [p/centered p/half-height p/line-bordered p/interactive-vi]
           {:align "left"
            :width "75%"
            :fg "blue"
            :selectedBg "blue"
            :selectedFg "white"}]
    :text [create-box
           [p/interactive-vi]
           {:scrollable true
            :alwaysScroll true
            :width "75%"
            :height "80%"
            :top "center"
            :left 5}]
    :preview [create-box
              []
              {:width "75%"
               :height "25%"
               :bottom 0
               :left "center"}]
    :row [create-box [] {:width "100%"}]
    :wrapper [create-box
              []
              {:parent (get-screen)
               :width "100%"
               :height "100%"}]
    :commandline [create-textbox
                  []
                  {:width "100%"
                   :bottom 0
                   :fg "yellow"}]))

(def set-args
  {:list set-items})

(def impl-map
  {:render render
   :render-deferred render-deferred
   :select (fn [widget i] (.select widget i))
   :prepend (fn [parent widget] (.prepend parent widget))
   :detach (fn [widget] (.detach widget))
   :set-content (fn [widget content] (.setContent widget content))
   :bind-global-keys bind-global-keys
   :set-items set-items
   :set-key-once set-key-once
   :set-keys set-keys
   :unset-key unset-key
   :unset-keys unset-keys
   :remove-all-listeners (fn [widget type] (.removeAllListeners widget type))
   :on (fn [widget type listener] (.on widget type listener))
   :create create
   :list-view list-view
   :create-text create-text
   :create-text-viewer create-text-viewer!})

(core/set-impl! impl-map)
