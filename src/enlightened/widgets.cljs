(ns enlightened.widgets
  (:refer-clojure :exclude [list])
  (:require [enlightened.core :as core]
            [enlightened.framework :as p]
            [enlightened.widgets.menu-impl :as menu-impl]))

(declare create-args set-args)

(defn types []
  [:list :preview :wrapper])

(defn create
  "Creates a widget. The overrides parameters are key-value pairs of a
  property and the desired override value.
  (See enlightened.widgets/types for valid type values.)"
  [type & overrides]
  (let [[create-fn mods props] (type create-args)]
    (create-fn overrides mods props)))

(defn set-content
  "Sets the content of a widget. Any content parameters will be
  concat'ed together to make up a list of the lines of the widget's
  body content.
  (See enlightened.widgets/types for valid type values.)"
  ([widget type title content1 content2 & contents]
     (set-content widget type title (apply concat content1 content2 contents)))
  ([widget type title content]
     (let [set-fn (type set-args)]
       (set-fn widget content)
       (when (seq title)
         (.prepend widget (core/create-text {:left 2 :content title}))))))

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
     (let [lst (list-view title content)]
       (.on lst "select" #(on-select %2 content))
       lst))
  ([title content]
     (let [view (create :wrapper :height "75%")
           lst (list title content)]
       (.append view lst)
       (.focus lst)
       (core/switch-active-view view)
       lst)))

(defn menu
  ([menu-items action-dispatch widget-hooks]
     (menu menu-items action-dispatch widget-hooks list))
  ([menu-items action-dispatch widget-hooks view-impl]
     (let [{menus :xs
            root :x} (group-by #(if (= 2 (count %)) :x :xs) menu-items)]
       (menu-impl/create-menu (first root)
                              (menu-impl/create-hierarchy root menus)
                              view-impl
                              action-dispatch
                              widget-hooks))))

(defn menu-view [menu-items action-dispatch widget-hooks]
  (menu menu-items action-dispatch widget-hooks list-view))

(def create-args
  {:list [core/create-list
          [p/centered p/half-height p/line-bordered p/interactive-vi]
          {:align "left"
           :width "75%"
           :fg "blue"
           :selectedBg "blue"
           :selectedFg "white"}]

   :preview [core/create-box
             []
             {:width "75%"
              :height "25%"
              :bottom 0
              :left "center"}]

   :wrapper [core/create-box
             []
             {:parent (core/get-screen)
              :width "100%"
              :height "100%"}]})

(def set-args
  {:list core/set-list})
