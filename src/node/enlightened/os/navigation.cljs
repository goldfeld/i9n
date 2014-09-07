(ns enlightened.os.navigation
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as a]
            [enlightened.nav-entry :as nav-entry]
            [enlightened.more :refer [channel? index-of]]
            [enlightened.os.term :as term :refer [widget?]]
            [enlightened.os.widgets :as widgets]))

(defmulti custom-nav-action
  (fn [action-map parse-fn-action nav channels]
    (:custom-nav-action action-map)))

(defmethod custom-nav-action :default
  [action-map parse-fn-action nav channels] nil)

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

(defn create-action! [options selection restore-state pane binds]
  (let [actions (take-nth 2 (rest options))]
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
                                            (take-nth 2 options) actions)))]
              (.detach book)
              (restore-state (or (:pos res) 0))
              (term/render-deferred)))
        nil)
      (nth actions selection))))

(defn create-selection-fn
  "Creates the pane/navigation selection fn to be passed to blessed's
  list internals. Here is all the logic dependant on what type of se"
  [widget {[id title options] :current :as nav} {in :in :as channels} cfg]
  (fn [_ i]
    (let [action (create-action! options i #(a/put! in [:hop id %])
                                 widget (:key-binds cfg))
          parse-fn-action
          (fn [act]
            (condp apply [act]
              keyword? (a/put! in [:next act i])
              sequential? (doseq [msg [(into [:add] act)
                                       [:next (ffirst act) i]]]
                            (a/put! in msg))
              string? (let [item (* 2 i)]
                        (a/put! in [:put
                                    [id item (str (get options item) ": " act)]
                                    [id (inc item) (constantly nil)]]))
              channel? (a/pipe act in false)
              nil? nil
              widget? (do (.detach widget) act)))]
      (condp apply [action]
        keyword? (a/put! in [:next action i])
        string? (let [text (create-text-viewer! widget action)]

                  [nil text {:remove-text #(do (.detach text) :do-once)}])

        channel? (a/pipe action in false)
        map? (custom-nav-action action parse-fn-action nav channels)
        fn? (parse-fn-action (action))
        ((:dispatch cfg) action)))))

(defn create-refresh-fn [widget title-widget channels cfg]
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
          (.on "select" (create-selection-fn widget nav channels cfg))))
      (term/render))
    (dissoc nav :rm-back)))

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
                 (fn [n [id place & fix]]
                   (let [apply-fix (nav-entry/create-fix place fix)
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
            nav)]
    (-> (assoc n
          :pos pos
          :current nav-entry
          :rm-back (:back nav)
          :back (when-let [parent (get-in n [:hierarchy id :link])]
                  #(a/put! in [:set (:nav-entry parent) (:pos parent)])))
        refresh)))

(def config-default
  {:dispatch identity
   :key-binds {:quit ["q" "escape"]
               :left ["h" "left"]
               :right ["l" "right"]}})

(defn create-pane
  ([current hierarchy widget]
     (create-pane current hierarchy widget config-default))
  ([[id title body :as current] hierarchy widget cfg]
     (let [in (or (:chan cfg) (a/chan))
           mult (or (:mult cfg) (a/mult in))
           channels (assoc (:watches cfg) :in in :mult mult)
           title-widget (term/create-text {:left 2 :content title})
           refresh (create-refresh-fn widget title-widget channels cfg)] 
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
                     (-> (assoc nav :pos current-pos)
                         (assoc-in [:hierarchy id :link]
                                   {:nav-entry current
                                    :pos current-pos}))
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
            :add
            (reduce (fn [n [id & more]]
                      (assoc-in n [:hierarchy id :data] (vec more)))
                    nav args)
            :stub
            (reduce (fn [n [id & more]]
                      (-> (assoc-in n [:hierarchy id :dirty] true)
                          (assoc-in [:hierarchy id :data] (vec more))))
                    nav args)
            :select (let [i (first args)]
                      (.select widget (if (= :last i)
                                        (-> nav :current (nth 2) count)
                                        i)))
            :dirty (reduce (fn [n id]
                             (assoc-in nav [:hierarchy id :dirty] true))
                           nav args)
            :state (update-in nav [:state] merge (apply hash-map args))))
        {:current current :pos 0 :hierarchy hierarchy}
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
                    (nav-entry/create-hierarchy (first root) entries)
                    list (merge config-default cfg)))))

(defn navigation-view
  ([nav-entries] (navigation-view nav-entries {}))
  ([nav-entries cfg] (navigation nav-entries cfg (widgets/list-view))))

(defn pick-option
  ([id title options settings]
     [id title (pick-option id options settings)])
  ([state-id options settings]
     (let [action (or (:action settings) (constantly (:next settings)))]
       (->>
        options
        (map (fn [option]
               {:custom-nav-action :pick-option
                :state-id state-id :option option
                :state-update-fn (:update settings)
                :action action :args {:pick option
                                      :pick-i (index-of options option)
                                      :options options}}))
        (interleave options)
        (#(concat % (:more settings)))))))

(defmethod custom-nav-action :pick-option
  [{:keys [state-id state-update-fn option action :as args]}
   parse-fn-action nav channels]
  (a/put! (:in channels) [:state state-id option])
  (->> (update-in (:state nav) [state-id]
                  (or state-update-fn (constantly option)))
       (assoc args :state)
       action parse-fn-action))
