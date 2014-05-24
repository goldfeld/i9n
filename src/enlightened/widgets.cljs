(ns enlightened.widgets
  (:require [enlightened.core :refer [get-screen switch-active-view
                                      create-text create-box create-form
                                      create-list set-list]]
            [enlightened.framework :as p]
            [enlightened.widgets.menu :refer [create-menu
                                              create-menu-hierarchy]]))

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
       (when (seq title) (.prepend widget (create-text {:left 2
                                                        :content title}))))))

(defn alert
  ([content] (alert [] content))
  ([overrides content] (alert 2000 overrides content))
  ([timeout overrides content]
     (js/setTimeout #(.log js/console content) 100)))

(defn list-view
  ([title content on-select]
     (let [list (list-view title content)]
       (.on list "select" #(on-select %2 content))
       list))
  ([title content]
     (let [view (create :wrapper :height "75%")
           list (create :list)]
       (.append view list)
       (.focus list)
       (set-content list :list title content)
       (switch-active-view view)
       list)))

(defn menu-view
  ([menu-items action-dispatch widget-hooks]
     (menu-view menu-items action-dispatch widget-hooks list-view))
  ([menu-items action-dispatch widget-hooks view-impl]
     (let [{menu :xs
            root :x} (group-by #(if (= 2 (count %)) :x :xs) menu-items)]
       (create-menu (first root)
                    (create-menu-hierarchy root menu)
                    view-impl
                    action-dispatch
                    widget-hooks))))

(def ^:private create-args
  {:list [create-list
          [p/centered p/half-height p/line-bordered p/interactive-vi]
          {:align "left"
           :width "75%"
           :fg "blue"
           :selectedBg "blue"
           :selectedFg "white"}]

   :preview [create-box
             []
             {:width "75%"
              :height "25%"
              :bottom 0
              :left "center"}]

   :wrapper [create-box
             []
             {:parent (get-screen)
              :width "100%"
              :height "100%"}]})

(def ^:private set-args
  {:list set-list})
