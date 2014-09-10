(ns enlightened.os.navigation
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as a]
            [enlightened.nav-entry :as nav-entry]
            [enlightened.more :refer [channel? index-of]]
            [enlightened.os.term :as term :refer [widget?]]
            [enlightened.os.widgets :as widgets]))

(defmulti custom-nav-action (fn [action-map more] (:nav-action action-map)))
(defmethod custom-nav-action :default [action-map more] nil)

(def nav-action? (every-pred map? #(contains? % :nav-action)))

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

(defn go-to-book-part [i chan book km key-binds titles parts]
  (apply term/unset-keys book @km)
  (.setContent book (nth parts i))
  (term/render)
  (let [quit (fn [] (a/put! chan [:pos i]) (a/close! chan))
        kmaps [(:quit key-binds) quit
               (:left key-binds)
               (if (> i 0)
                 #(go-to-book-part (dec i) chan book km key-binds titles parts)
                 quit)
               (:right key-binds)
               (if (< i (dec (count parts)))
                 #(go-to-book-part (inc i) chan book km key-binds titles parts)
                 (constantly nil))]]
    (reset! km kmaps)
    (apply term/set-keys book kmaps)
    chan))

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
  [widget {[id title body] :current :as nav}
   handle-returned-action {in :in :as channels} cfg]
  (fn [_ i]
    (let [action (create-action! body i #(a/put! in [:hop id %])
                                 widget (:key-binds cfg))]
      (condp apply [action]
        keyword? (a/put! in [:next action i])
        string? (let [text (create-text-viewer! widget action)]

                  [nil text {:remove-text #(do (.detach text) :do-once)}])

        channel? (a/pipe action in false)
        nav-action?
        (custom-nav-action
         action {:selected i :nav nav :channels channels
                 :handle-returned-action
                 #(handle-returned-action % i handle-returned-action nav)})
        fn? (handle-returned-action (action) i handle-returned-action nav)
        ((:dispatch cfg) action)))))

(defn create-refresh-fn
  [widget title-widget handle-returned-action channels cfg]
  (fn [{back :back rm-back :rm-back [id title bd] :current :as nav}]
    (let [l-binds (-> cfg :key-binds :left)
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
          (.on "select" (create-selection-fn widget nav handle-returned-action
                                             channels cfg))))
      (term/render))
    (dissoc nav :rm-back)))

(defn create-handle-returned-action [{in :in :as channels} widget]
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
      nav-action?
      (custom-nav-action action {:selected i :nav nav :channels channels
                                 :handle-returned-action #(self % i self nav)})
      nil? nil
      widget? (do (.detach widget) action))))

(defn change
  "Applies :fix or :put operations into a nav. The option between :fix
  and :put is made by the persist? parameter, which means that a :fix
  is persisted into the hierarchy, whereas a :put is not. Meanwhile,
  operations targeting the current nav-entry will effect the
  temporary-state nav-entry copy, regardless of the persist? param. As
  such, each operation may in fact be applied to both hierarchy and
  current entry, to one of the two, or to none at all."
  [nav args refresh persist?]
  (let [current-id (-> nav :current first)
        new-nav (reduce
                 (fn [n [target place & fix]]
                   (let [id (or target current-id)
                         apply-fix (nav-entry/create-fix place fix)
                         n' (if persist?
                              (apply-fix n [:hierarchy id :data 0]
                                         [:hierarchy id :data 1])
                              n)]
                     (if (= id current-id)
                       (-> (apply-fix n' [:current 1] [:current 2])
                           (assoc :current-is-dirty true))
                       n')))
                 nav args)
        n (dissoc new-nav :current-is-dirty)]
    (if (:current-is-dirty new-nav) (refresh n) n)))

(defn hop [nav-entry pos nav {:keys [in flush]} refresh]
  (let [id (first nav-entry)
        n (if (and flush (get-in nav [:hierarchy id :dirty]))
            (do (a/put! flush id) (assoc-in nav [:hierarchy id :dirty] false))
            nav)
        n' (if-let [trigger (get-in n [:hierarchy id :trigger])]
             (do (trigger id in)
                 (update-in n [:hierarchy id] dissoc :trigger))
             n)]
    (-> (assoc n'
          :pos pos
          :current nav-entry
          :rm-back (:back n')
          :back (when-let [parent (get-in n' [:hierarchy id :link])]
                  #(a/put! in [:set (:nav-entry parent) (:pos parent)])))
        refresh)))

(def config-default
  {:dispatch identity
   :key-binds {:quit ["q" "escape"]
               :left ["h" "left"]
               :right ["l" "right"]}})

(defn update-state [nav state-id state-val]
  (if-let [{:keys [set deps]} (get-in nav [:hierarchy :state state-id])]
    (update-in nav [:hierarchy :state state-id :val]
               (fn [old-val]
                 (set state-val old-val 
                      (select-keys (get-in nav [:hierarchy :state]) deps))))
    (assoc-in nav [:hierarchy :state state-id]
              {:val state-val
               :get (fn [val] val)
               :set (fn [val _ _] val)})))

(defn update-states [nav & id-state-pairs]
  (reduce (fn [n [state-id state-val]]
            (if-let [dpdnts (get-in n [:hierarchy :state state-id :dependants])]
              (reduce (fn [n' dpdt]
                        (condp apply [dpdt]
                          fn? (do (dpdt) n')
                          keyword? (let [d (get-in n' [:hierarchy :state dpdt])]
                                     (or (and (map? d) (= :eager (:computed d))
                                              (update-state n' dpdt (:val d)))
                                         n'))))
                      (update-state n state-id state-val)
                      dpdnts)
              (update-state n state-id state-val)))
          nav (partition 2 id-state-pairs)))

(defn create-pane
  ([current initial-nav widget]
     (create-pane current initial-nav widget config-default))
  ([[id title body :as current] initial-nav widget cfg]
     (let [in (or (:chan cfg) (a/chan))
           mult (or (:mult cfg) (a/mult in))
           channels (assoc (:watches cfg) :in in :mult mult)
           title-widget (term/create-text {:left 2 :content title})
           handle-returned-action (create-handle-returned-action channels
                                                                 widget)
           refresh (create-refresh-fn widget title-widget
                                      handle-returned-action channels cfg)]
       (.prepend widget title-widget)
       (a/reduce
        (fn [nav [cmd & args]]
          (case cmd
            :next
            (let [[id pos go-to] args
                  current (:current nav)
                  current-pos (or pos 0)
                  dest (get-in nav [:hierarchy id :data])]
              (if (and dest (not= id (first current)))
                (hop (into [id] dest) (or go-to 0)
                     (assoc-in nav [:hierarchy id :link]
                               {:nav-entry current
                                :pos current-pos})
                     channels refresh)
                nav))
            :hop
            (let [[id go-to] args
                  dest (get-in nav [:hierarchy id :data])]
              (if-not dest
                nav
                (hop (into [id] dest) (or go-to 0) nav channels refresh)))
            :set
            (let [[nav-entry go-to] args]
              (hop nav-entry (or go-to 0) nav channels refresh))
            :fix (change nav args refresh :persist)
            :put (change nav args refresh false)
            :add (nav-entry/add-to-hierarchy nav args)
            :stub (nav-entry/add-to-hierarchy
                   nav args #(assoc-in %1 [:hierarchy %2 :dirty] true))
            :select (let [i (first args)]
                      (.select widget (if (= :last i)
                                        (-> nav :current (nth 2) count)
                                        i)))
            :dirty (reduce (fn [n id]
                             (assoc-in nav [:hierarchy id :dirty] true))
                           nav args)
            :state (apply update-states nav args)
            :nav-action-with-state
            (let [[action i action-args] args]
              (handle-returned-action
               (action (assoc action-args :state (:state nav)))
               i handle-returned-action nav)
              nav)))
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
    (a/put! [:nav-action-with-state action selected args])))

(defmethod custom-nav-action :hop
  [{:keys [action args]} {:keys [channels]}]
  (a/put! (:in channels) [:hop action]))
