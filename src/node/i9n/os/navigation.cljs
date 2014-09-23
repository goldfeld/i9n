(ns i9n.os.navigation
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as a]
            [secretary.core :as secretary]
            [i9n.ext :as ext]
            [i9n.nav-entry :as nav-entry]
            [i9n.more :refer [channel? index-of]]
            [i9n.step :refer [set-route-dispatch!]]
            [i9n.os.term :as term :refer [widget?]]
            [i9n.os.widgets :as widgets]))

(defmulti custom-nav-action (fn [action-map more] (:nav-action action-map)))
(set-route-dispatch! secretary/dispatch!)

(defmethod custom-nav-action :default [action-map more] nil)

(declare create-pane)

(defn create-text-viewer!
  ([pane text]
     (let [viewer (create-text-viewer! pane)]
       (.setContent viewer text)
       viewer))
  ([pane]
     (let [viewer (widgets/text)]
       (term/set-items pane [])
       (.append pane viewer)
       (.focus viewer)
       viewer)))

(defn go-to-book-part [i chan book km keybinds titles parts]
  (apply term/unset-keys book @km)
  (.setContent book (nth parts i))
  (term/render)
  (let [quit (fn [] (a/put! chan [:pos i]) (a/close! chan))
        kmaps [(:quit keybinds) quit
               (:left keybinds)
               (if (> i 0)
                 #(go-to-book-part (dec i) chan book km keybinds titles parts)
                 quit)
               (:right keybinds)
               (if (< i (dec (count parts)))
                 #(go-to-book-part (inc i) chan book km keybinds titles parts)
                 (constantly nil))]]
    (reset! km kmaps)
    (apply term/set-keys book kmaps)
    chan))

(defn handle-map-action
  [action i {in :in}]
  (condp #(contains? %2 %1) action
    :nav-action (a/put! in [:nav-action action i])
    :i9n (a/put! in [:i9n action]) 
    nil))

(defn create-action! [body selection restore-state pane binds]
  (let [actions (take-nth 2 (rest body))]
    (if (every? string? actions)
      (let [book (create-text-viewer! pane)
            book-chan (a/chan)]
        (go (let [res (<! (a/reduce
                           (fn [m msg] (case (first msg)
                                         :pos (assoc m :pos (second msg))
                                         m))
                           {}
                           (go-to-book-part selection book-chan book
                                            (atom nil) binds
                                            (take-nth 2 body) actions)))]
              (.detach book)
              (restore-state (or (:pos res) 0))
              (term/render-deferred)))
        nil)
      (nth actions selection))))

(defn create-selection-fn
  "Creates the pane/navigation selection fn to be passed to blessed's
  list internals. Here is all the logic dependant on what type of se"
  [widget {[id title body] :current :as nav} hra {in :in :as channels} cfg]
  (fn [_ i]
    (let [action (create-action! body i #(a/put! in [:hop id %])
                                 widget (:keybinds cfg))]
      (condp apply [action]
        keyword? (a/put! in [:next action i])
        string? (let [text (create-text-viewer! widget action)]

                  [nil text {:remove-text #(do (.detach text) :do-once)}])

        channel? (a/pipe action in false)
        map? (handle-map-action action i channels)
        fn? (hra (action) i hra nav)
        ((:dispatch cfg) action)))))

(defn create-refresh-fn
  [widget title-widget hra channels cfg]
  (fn [{back :back rm-back :rm-back [id title bd] :current :as nav}]
    (let [l-binds (-> cfg :keybinds :left)
          parse-body
          (fn [body]
            (condp apply [body]
              vector? body
              sequential? (vec body)
              string? (let [t (create-text-viewer! widget body)
                            b (or back (constantly nil))]
                        (term/set-key-once t l-binds #(do (.detach t) (b)))
                        nil)
              channel? (do (a/pipe body (:in channels) false) nil)
              map? (handle-map-action body nil channels)
              fn? (recur (body))
              nil))
          options (parse-body bd)]
      (when rm-back (term/unset-key widget l-binds rm-back))
      (when back (term/set-key-once widget l-binds back))
      (.setContent title-widget title)
      (.removeAllListeners widget "select")
      (when options 
        (doto widget
          (term/set-items (take-nth 2 options))
          (.select (:pos nav))
          (.on "select" (create-selection-fn widget nav hra channels cfg))))
      (term/render))
    (dissoc nav :rm-back)))

(defn create-handle-returned-action [{in :in :as channels} widget cfg]
  (fn [action i self {[id title body] :current :as nav}]
    (condp apply [action]
      keyword? (a/put! in [:next action i])
      sequential? (do (a/put! in (into [:add] action))
                      (a/put! in [:next (ffirst action) i]))
      string? (let [item (* 2 i)]
                (a/put! in [:put
                            [id item (str (get body item) ": " action)]
                            [id (inc item) (constantly nil)]]))
      channel? (a/pipe action in false)
      map? (handle-map-action action i channels)
      nil? nil
      widget? (do (.detach widget) action))))

(def config-default
  {:dispatch identity
   :keybinds {:quit ["q" "escape"]
              :left ["h" "left"]
              :right ["l" "right"]}})

(defn create-pane
  ([current initial-nav widget]
     (create-pane current initial-nav widget config-default))
  ([[id title body :as current] initial-nav widget cfg]
     (let [in (or (:chan cfg) (a/chan))
           mult (or (:mult cfg) (a/mult in))
           channels (assoc (:watches cfg) :in in :mult mult)
           title-widget (term/create-text {:left 2 :content title})
           hra (create-handle-returned-action channels widget cfg)
           other {:put! a/put! :render! term/render :widget widget
                  :channels channels :handle-returned-action hra
                  :refresh (create-refresh-fn widget title-widget
                                              hra channels cfg)}]
       (doto widget
         (term/set-keys
          "k" #(a/put! in [:select - 1])
          "j" #(a/put! in [:select + 1]))
         (.prepend title-widget))
       (a/reduce
        (fn [nav [cmd & args :as op]]
          (case cmd
            :i9n (do (ext/custom-i9n (first args)
                                     {:parent widget :nav nav :cfg cfg})
                     nav)
            :nav-action
            (let [[action-map i] args]
              (custom-nav-action
               action-map {:selected i :nav nav :channels channels
                           :handle-returned-action #(hra % i hra nav)})
              nav)
            :handle-returned-action
            (let [[action action-args i] args]
              (hra (action (assoc action-args :state (:state nav))) i hra nav)
              nav)
            (ext/i9n-op op nav other)))
        (assoc initial-nav :current current :pos 0)
        (a/tap mult (a/chan)))
       (a/put! in [:hop id]))
     widget))

(defn navigation
  ([nav-entries] (navigation nav-entries {}))
  ([nav-entries cfg] (navigation nav-entries cfg (widgets/create :list)))
  ([nav-entries cfg list]
     (let [{entries :xs
            root :x} (group-by #(if (= 2 (count %)) :x :xs) nav-entries)]
       (create-pane (cons :root (first root))
                    (nav-entry/create-nav (first root) entries)
                    list (merge config-default cfg)))))

(defn navigation-view
  ([nav-entries] (navigation-view nav-entries {}))
  ([nav-entries cfg] (navigation nav-entries cfg (widgets/list-view))))

(defn pick-option
  ([id title options settings]
     [id title (pick-option id options settings)])
  ([state-id options settings]
     (let [action (or (:action settings)
                      (constantly (or (:next settings)
                                      (when-let [hop (:hop settings)]
                                        {:nav-action :hop :action hop}))))]
       (->>
        options
        (map (fn [option]
               (let [i (index-of options option)]
                 {:nav-action :pick-option
                  :action action
                  :args {:pick (if-let [handles (:handles settings)]
                                 (nth handles i)
                                 option)
                         :pick-i i
                         :state-id state-id
                         :options options}})))
        (interleave options)
        (#(concat % (:more settings)))))))

(defmethod custom-nav-action :pick-option
  [{:keys [action args]} {:keys [selected channels]}]
  (doto (:in channels)
    (a/put! [:state (:state-id args) (:pick args)])
    (a/put! [:handle-returned-action action args selected])))

(defmethod custom-nav-action :hop
  [{:keys [action args]} {:keys [channels]}]
  (a/put! (:in channels) [:hop action]))
