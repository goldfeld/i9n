(ns i9n.ui
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as a]
            [secretary.core :as secretary]
            [i9n.ext :refer [custom-i9n-op]]
            [i9n.nav-entry :as nav-entry]
            [i9n.keymap :as keymap]
            [i9n.more :refer [channel? widget?]]))

(defn handle-map-action
  [action i {in :in}]
  (condp #(contains? %2 %1) action
    :nav-action (a/put! in [:nav-action action i])
    :i9n (a/put! in [:i9n action])
    nil))

(defn go-to-book-part [i chan book km keybinds impl titles parts]
  (apply (:unset-keys impl) book @km)
  ((:set-content impl) book (nth parts i))
  ((:render impl))
  (let [quit (fn [] (a/put! chan [:pos i]) (a/close! chan))
        kmaps [(:quit keybinds) quit
               (:left keybinds)
               (if (> i 0)
                 #(go-to-book-part (dec i) chan book km
                                   keybinds impl titles parts)
                 quit)
               (:right keybinds)
               (if (< i (dec (count parts)))
                 #(go-to-book-part (inc i) chan book km
                                   keybinds impl titles parts)
                 (constantly nil))]]
    (reset! km kmaps)
    (apply (:set-keys impl) book kmaps)
    chan))

(defn create-action! [body selection restore-state pane binds impl]
  (let [actions (take-nth 2 (rest body))]
    (if (every? string? actions)
      (let [book ((:create-text-viewer impl) pane)
            book-chan (a/chan)]
        (go (let [res (<! (a/reduce
                           (fn [m msg] (case (first msg)
                                         :pos (assoc m :pos (second msg))
                                         m))
                           {}
                           (go-to-book-part selection book-chan book
                                            (atom nil) binds impl
                                            (take-nth 2 body) actions)))]
              ((:detach impl) book)
              (restore-state (or (:pos res) 0))
              ((:render-deferred impl))))
        nil)
      (nth actions selection))))

(defn create-handle-returned-action [{in :in :as channels} widget cfg impl]
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
      widget? (do ((:detach impl) widget) action))))

(defn create-pick-fn
  [widget hra {in :in :as channels} cfg impl]
  (fn [i {[id title body] :current :as nav}]
    (let [action (create-action! body i #(a/put! in [:hop id %])
                                 widget (:keybinds cfg) impl)]
      (condp apply [action]
        keyword? (a/put! in [:next action i])
        string? ((:create-text-viewer impl) widget action)
        channel? (a/pipe action in false)
        map? (handle-map-action action i channels)
        fn? (hra (action) i hra nav)
        nil))))

(defn preprocess-body [body]
  (reduce (fn [coll [label action]]
            (case label
              :skip-lines (into coll (repeat (* 2 action) nil))
              (conj coll label action)))
          []
          (partition 2 body)))

(defn create-refresh-fn
  [widget title-widget hra channels cfg impl]
  (fn [{back :back rm-back :rm-back [id title bd] :current :as nav}]
    (let [l-binds (-> cfg :keybinds :left)
          parse-body
          (fn [body]
            (condp apply [body]
              vector? (preprocess-body body)
              sequential? (vec body)
              string? (let [t ((:create-text-viewer impl) widget body)
                            b (or back (constantly nil))]
                        ((:set-key-once impl) t l-binds
                         #(do ((:detach impl) t) (b)))
                        nil)
              channel? (do (a/pipe body (:in channels) false) nil)
              map? (handle-map-action body nil channels)
              fn? (recur (body))
              nil))
          options (parse-body bd)]
      (when rm-back ((:unset-key impl) widget l-binds rm-back))
      (when back ((:set-key-once impl) widget l-binds back))
      ((:set-content impl) title-widget (if (string? title) title "[untitled]"))
      ((:remove-all-listeners impl) widget "select")
      (when options
        (doto widget
          ((:set-items impl) (take-nth 2 options))
          ((:select impl) (:pos nav))))
      ((:render impl)))
    (-> (assoc nav :pick (create-pick-fn widget hra channels cfg impl))
        (dissoc :rm-back))))

(def config-default
  {:keybinds {:quit ["q" "escape"]
              :left ["h" "left"]
              :right ["l" "right"]
              :page-up ["C-p"]
              :page-down ["C-n"]
              :cut-item ["d"]
              :paste-below ["p"]
              :paste-above ["P"]}})

(defn make-create-pane-impl [impl]
  (fn create-pane
    ([current initial-nav widget]
       (create-pane current initial-nav widget config-default))
    ([[id title body :as current] initial-nav widget cfg]
       (let [in (or (:chan cfg) (a/chan))
             mult (or (:mult cfg) (a/mult in))
             channels (assoc (:watches cfg) :in in :mult mult)
             title-widget ((:create-text @impl) {:left 2 :content title})
             hra (create-handle-returned-action channels widget cfg @impl)
             other {:widget widget :cfg cfg :impl @impl
                    :render! (:render @impl) :select! (:select @impl)
                    :channels channels :handle-returned-action hra
                    :refresh (create-refresh-fn widget title-widget
                                                hra channels cfg @impl)}]
         (doto widget
           ((:on @impl) "keypress"
            (fn [ch keydata]
              (a/put! in [:key (clojure.string/lower-case
                                (aget keydata "full"))])))
           ((:prepend @impl) title-widget))
         (a/reduce
          (fn [{last-op :last-op :as nav} op]
            (let [timestamp (.getTime (js/Date.))]
              (-> (custom-i9n-op op nav other)
                  (assoc-in [:history timestamp]
                            {:op op :nav (dissoc nav :history)
                             :next [] :prev [last-op]})
                  (update-in [:history last-op :next] conj timestamp)
                  (assoc :last-op timestamp))))
          (assoc initial-nav :current current :pos 0
                 :keymap keymap/vi :keystate []
                 :last-op :start :history {:start {:next []}})
          (a/tap mult (a/chan)))
         (a/put! in [:hop id]))
       widget)))

(defn make-navigation-impl [impl create-pane-impl]
  (fn navigation
    ([nav-entries] (navigation nav-entries {}))
    ([nav-entries cfg] (navigation nav-entries cfg ((:create @impl) :list)))
    ([nav-entries cfg list]
       (let [{entries :xs
              root :x} (group-by #(if (and (vector? %) (= 2 (count %))) :x :xs)
                                 nav-entries)]
         (create-pane-impl (cons :root (first root))
                           (nav-entry/create-nav (first root) entries)
                           list (merge config-default cfg))))))

(defn make-navigation-view-impl [impl navigation-impl]
  (fn navigation-view
    ([nav-entries] (navigation-view nav-entries {}))
    ([nav-entries cfg]
       (navigation-impl nav-entries cfg ((:list-view @impl))))))
