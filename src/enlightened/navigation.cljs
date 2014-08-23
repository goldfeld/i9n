(ns enlightened.navigation
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as a]
            [enlightened.core :as core :refer [channel? widget?]]
            [enlightened.widgets :as widgets]))

(declare create-pane)

(defn create-hierarchy [root-item nav-entries]
  (reduce (fn [m x] (assoc m (keyword (first x)) (vec (rest x))))
          {:root root-item} nav-entries))

(defn create-text-viewer!
  ([pane text]
     (let [viewer (create-text-viewer! pane)]
       (.setContent viewer text)
       viewer))
  ([pane]
     (let [viewer (widgets/text)]
       (core/set-items pane [])
       (.append pane viewer)
       (.focus viewer)
       viewer)))

(defn go-to-book-part [i chan book km key-binds titles parts]
  (apply core/unset-keys book @km)
  (.setContent book (nth parts i))
  (core/render)
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
    (apply core/set-keys book kmaps)
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
              (core/render-deferred)))
        nil)
      (nth actions selection))))

(defn create-selection-fn
  "Creates the pane/navigation selection fn to be passed to blessed's
  list internals. Here is all the logic dependant on what type of se"
  [widget [id title options] {in :in :as channels} cfg]
  (fn [_ i]
    (let [action (create-action! options i #(a/put! in [:hop id %])
                                 widget (:key-binds cfg))]
      (condp apply [action]
        keyword? (a/put! in [:next action i])
        string? (let [text (create-text-viewer! widget action)]

                  [nil text {:remove-text #(do (.detach text) :do-once)}])

        channel? (a/pipe action in false)
        fn?
        (let [res (action)]
          (condp apply [res]
            keyword? (a/put! in [:next res i])
            vector? (doseq [msg [(into [:add] res)
                                 [:next (ffirst res) i]]]
                      (a/put! in msg))
            string? (let [item (* 2 i)]
                      (a/put! in [:put
                                  [id item (str (get options item) ": " res)]
                                  [id (inc item) (constantly nil)]]))
            channel? (a/pipe res in false)
            nil? nil
            widget? (do (.detach widget) res)))
        ((:dispatch cfg) action)))))

(defn create-refresh-fn [widget title-widget channels cfg]
  (fn [nav]
    (let [[id title b :as current] (:current nav)
          parse-body
          (fn [body]
            (condp apply [body]
              string? (let [text (create-text-viewer! widget body)]
                        (core/set-key-once
                         text (-> cfg :key-binds :left)
                         #(do (.detach text) (when-let [bk (:back nav)] (bk))))
                        nil)
              channel? (do (a/pipe body (:in channels) false) nil)
              fn? (recur (body))
              body))
          options (parse-body b)
          left-binds (-> cfg :key-binds :left)]
      (when-let [rm (:rm-back nav)] (core/unset-key widget left-binds rm))
      (when-let [back (:back nav)] (core/set-key-once widget left-binds back))
      (.setContent title-widget title)
      (.removeAllListeners widget "select")
      (when options 
        (doto widget
          (core/set-items (take-nth 2 options))
          (.select (:pos nav))
          (.on "select" (create-selection-fn widget current channels cfg))))
      (core/render))
    (dissoc nav :rm-back)))

(defn apply-fix [nav path place title-idx body-idx fix]
  (if (= place :title)
    (assoc-in nav (conj path title-idx) fix)
    (let [bpath (conj path body-idx)
          body (get-in nav bpath)]
      (assoc-in nav bpath (assoc (if (vector? body) body [])
                            place fix)))))

(defn change [nav args refresh persist?]
  (let [current-id (-> nav :current first)
        new-nav (reduce
                 (fn [n [id place fix]]
                   (let [n' (if persist?
                              (apply-fix n [:hierarchy id] place 0 1 fix)
                              n)]
                     (if (= id current-id)
                       (-> (apply-fix n' [:current] place 1 2 fix)
                           (assoc :current-is-dirty true))
                       n')))
                 nav args)
        n (dissoc new-nav :current-is-dirty)]
    (if (:current-is-dirty new-nav) (refresh n) n)))

(defn hop [nav-entry pos nav {:keys [in flush]} refresh]
  (let [id (first nav-entry)
        n (if (and flush (get-in nav [:dirty id]))
            (do (a/put! flush id) (assoc-in nav [:dirty id] false))
            nav)]
    (-> (assoc n
          :pos pos
          :current nav-entry
          :rm-back (:back n)
          :back (when-let [parent (get-in n [:links id])]
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
           out (a/tap (a/mult in) (a/chan))
           channels (assoc (:watches cfg) :in in :out out)
           title-widget (core/create-text {:left 2 :content title})
           refresh (create-refresh-fn widget title-widget channels cfg)] 
       (.prepend widget title-widget)
       (a/reduce
        (fn [nav [cmd & args]]
          (case cmd
            :next
            (let [[id pos go-to] args
                  current (:current nav)
                  current-pos (or pos 0)
                  dest (get-in nav [:hierarchy id])]
              (if (and dest (not= id (first current)))
                (hop (into [id] dest) (or go-to 0)
                     (-> (assoc nav :pos current-pos)
                         (assoc-in [:links id]
                                   {:nav-entry current
                                    :pos current-pos}))
                     channels refresh)
                nav))
            :hop
            (let [[id go-to] args
                  dest (get-in nav [:hierarchy id])]
              (if-not dest
                nav
                (hop (into [id] dest) (or go-to 0) nav channels refresh)))
            :set
            (let [[nav-entry go-to] args]
              (hop nav-entry (or go-to 0) nav channels refresh))
            :fix (change nav args refresh :persist)
            :put (change nav args refresh false)
            :add
            (reduce (fn [n [id & more]] (assoc-in n [:hierarchy id] (vec more)))
                    nav args)
            :stub
            (reduce (fn [n [id & more]]
                      (-> (assoc-in n [:dirty id] true)
                          (assoc-in [:hierarchy id] (vec more))))
                    nav args)
            :dirty (assoc-in nav [:dirty (first args)] true)))
        {:current current :pos 0 :hierarchy hierarchy}
        out)
       (a/put! in [:hop id]))
     widget))

(defn navigation
  ([nav-entries] (navigation nav-entries {}))
  ([nav-entries cfg] (navigation nav-entries cfg (widgets/create :list)))
  ([nav-entries cfg list]
     (let [{entries :xs
            root :x} (group-by #(if (= 2 (count %)) :x :xs) nav-entries)]
       (create-pane (cons :root (first root))
                    (create-hierarchy (first root) entries)
                    list (merge config-default cfg)))))

(defn navigation-view
  ([nav-entries] (navigation-view nav-entries {}))
  ([nav-entries cfg] (navigation nav-entries cfg (widgets/list-view))))
