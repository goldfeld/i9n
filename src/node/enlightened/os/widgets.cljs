(ns enlightened.os.widgets
  (:refer-clojure :exclude [list])
  (:require [enlightened.properties :as p]
            [enlightened.os.term :as term]))

(declare create-args set-args)

(defn types []
  [:list :text :preview :row :wrapper :commandline])

(defn create
  "Creates a widget. The overrides parameters are key-value pairs of a
  property and the desired override value.
  (See enlightened.widgets/types for valid type values.)"
  [type & overrides]
  (let [[create-fn mods props] (create-args type)]
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
       (term/set-title widget title))))

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
       (term/switch-active-view view widget)
       widget)))

(defn text
  ([title txt]
     (let [widget (text txt)]
       (term/set-title widget title)
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
    (term/set-title widget title)
    (term/switch-active-view view widget)
    widget))

(defn create-args [type]
  (case type
    :list [term/create-list
           [p/centered p/half-height p/line-bordered p/interactive-vi]
           {:align "left"
            :width "75%"
            :fg "blue"
            :selectedBg "blue"
            :selectedFg "white"}]
    :text [term/create-box
           [p/interactive-vi]
           {:scrollable true
            :alwaysScroll true
            :width "75%"
            :height "80%"
            :top "center"
            :left 5}]
    :preview [term/create-box
              []
              {:width "75%"
               :height "25%"
               :bottom 0
               :left "center"}]
    :row [term/create-box [] {:width "100%"}]
    :wrapper [term/create-box
              []
              {:parent (term/get-screen)
               :width "100%"
               :height "100%"}]
    :commandline [term/create-textbox
                  []
                  {:width "100%"
                   :bottom 0
                   :fg "yellow"}]))

(def set-args
  {:list term/set-items})
