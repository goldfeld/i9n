(ns i9n.core
  (:require-macros [i9n.ext :refer [defop]])
  (:require [cljs.core.async :as a]
            [secretary.core :as secretary]
            [flow.datetime :as dt]
            [i9n.op.state :as op-state]
            [i9n.op.fix :as op-fix]
            [i9n.op.hop :as op-hop]
            [i9n.keymap :as keymap]
            [i9n.ui :as ui]
            [i9n.ext :as ext]
            [i9n.nav-entry :as nav-entry]
            [i9n.more :refer [index-of]]))

;; set-impl! is called from os-specific code (e.g. node.js)
(def impl (atom nil))
(defn set-impl! [impl-map] (reset! impl impl-map))
(def create-pane (ui/make-create-pane-impl impl))
(def navigation (ui/make-navigation-impl impl create-pane))
(def navigation-view (ui/make-navigation-view-impl impl navigation))

;;; UI HELPERS

(defn editable [& nav-entry]
  {:i9n-step :editable
   :entry nav-entry})

(defn pick-option
  ([id title options settings]
     [id title (pick-option id options settings)])
  ([state-id options settings]
     (let [action (or (:action settings)
                      (constantly (or (:next settings)
                                      (when-let [hop (:hop settings)]
                                        {:i9n-action :hop :action hop}))))]
       (->>
        options
        (map (fn [option]
               (let [i (index-of options option)]
                 {:i9n-action :pick-option
                  :action action
                  :args {:pick (if-let [handles (:handles settings)]
                                 (nth handles i)
                                 option)
                         :pick-i i
                         :state-id state-id
                         :options options}})))
        (interleave options)
        (#(concat % (:more settings)))))))

;;; ACTIONS

(defmethod ext/custom-i9n-action :pick-option
  [{:keys [action args]} {:keys [selected channels]}]
  (doto (:in channels)
    (a/put! [:state (:state-id args) (:pick args)])
    (a/put! [:handle-returned-action action args selected])))

(defmethod ext/custom-i9n-action :hop
  [{:keys [action]} {{in :in} :channels}]
  (a/put! in [:hop action]))

(def n-fn-arg-paths
  {:state [:nav :state]
   :in [:channels :in]
   :nav [:nav] :put [:put]})

(defmethod ext/custom-i9n-action :n-fn
  [{:keys [action requested-args]} {:keys [handle-returned-action] :as more}]
  (handle-returned-action
   (action (reduce (fn [m req-arg]
                     (assoc m req-arg
                            (get-in more (n-fn-arg-paths req-arg))))
                   {} requested-args))))

;;; OPERATIONS

(defop add [[cmd & args] nav more] {}
  (nav-entry/add-to-hierarchy nav args))

(defop stub [[cmd & args] nav more] {}
  (nav-entry/add-to-hierarchy
   nav args #(assoc-in %1 [:hierarchy %2 :dirty] true)))

(defn select-option
  ([widget render! select! nav i relative-fn]
     (select-option widget render! select! nav (relative-fn (:pos nav) i)))
  ([widget render! select! {last :last :as nav} i]
     (if-let [k (cond (= :last i) last
                      (integer? i) (cond (> i last) last
                                         (< i 0) 0
                                         :else i))]
       (do (select! widget k)
           (render!)
           (assoc nav :pos k))
       nav)))

(defop select [[cmd & args] nav {:keys [widget render! select!]}] {}
  (apply select-option widget render! select! nav
         (if (= 1 (count args)) args (reverse args))))

(defop dirty [[cmd & args] nav more] {}
  (reduce (fn [n id] (assoc-in nav [:hierarchy id :dirty] true))
          nav args))

(defop fix [[cmd & args] nav more] {}
  (op-fix/change nav args (:refresh more) :persist false))

(defop user-fix [[cmd & args] nav more] {}
  (op-fix/change nav args (:refresh more) :persist :user-made))

(defop put [[cmd & args] nav more] {}
  (op-fix/change nav args (:refresh more) false false))

(defop toggle-editable [[cmd id toggle] nav more] {}
  (let [target (or id (-> nav :current first))]
    (if (nil? toggle)
      (update-in nav [:hierarchy id :editable] not)
      (assoc-in nav [:hierarchy id :editable] toggle))))

(defop state [[cmd & args] nav more] {}
  (apply op-state/update-states nav args))

(defop history [[cmd & args] nav {{in :in} :channels}] {}
  (a/put!
   in [:next [:i9n-history "Navigation history"
              (-> (reduce-kv (fn [labels timestamp {:keys [prev op n]}]
                               (if (some #{:next :hop :set} [op])
                                 (conj labels
                                       (str (dt/time-display timestamp)
                                            " - " (first (:current n))))
                                 labels))
                             {} (:history nav))
                  (interleave (repeat nil)))]])
  nav)

(defop undo
  [[cmd & args] {:keys [last-op history] :as nav} {:keys [refresh]}] {}
  (let [undo-candidates (get-in history [last-op :prev])
        prev-nav (get-in history [(apply max undo-candidates) :nav])]
    (refresh (assoc prev-nav :history history))))

;; {{{
;; }}} ;;;;;;;;;;;;;;;;;;
;; {{{ :next operation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defop next [[cmd & args] nav more] {}
  (let [create-link (fn [n id] (assoc-in n [:hierarchy id :link]
                                         {:nav-entry (:current nav)
                                          :pos (nth args 1 0)}))]
    (op-hop/may-hop (first args) nav
                    {:may-create-link create-link
                     :do-hop #(op-hop/hop %1 (nth args 2 0) %2 more)
                     :may-abort
                     (fn [id n do-hop]
                       (if (not= id (first (:current n))) (do-hop) n))})))

;; }}} ;;;;;;;;;;;;;;;;;
;; {{{ :hop operation ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defop hop [[cmd & args] nav more] {}
  (op-hop/may-hop (first args) nav
                  {:may-create-link (fn [n id] n)
                   :do-hop #(op-hop/hop %1 (nth args 1 0) %2 more)
                   :may-abort (fn [id _ do-hop] (do-hop))}))

(defop set [[cmd nav-entry go-to] nav more] {}
  (op-hop/hop nav-entry (or go-to 0) nav more))

;; }}} ;;;;;;;;;;;;;;
;; :back operation ;;
;;;;;;;;;;;;;;;;;;;;;

(defop back
  [[cmd times] {back :back :as nav} {{in :in} :channels}] {}
  (when back
    (let [x (or times 1)]
      (cond
       (= 1 x) (back)
       (and (integer? x) (> x 0)) (do (back)
                                      (a/put! in [:back (dec x)]))
       :else (throw (js/Error. (str "custom-i9n-op :back takes only"
                                    "an optional positive integer."))))))
  nav)

(defop pick
  [[cmd i] {:keys [pick pos] :as nav} {:keys [channels]}] {}
  (when pick (pick (or i pos) nav))
  nav)

(defop key [[cmd & args] nav {:keys [channels]}] {}
  (let [[action keystate] (keymap/handle-key (first args)
                                             (:keystate nav)
                                             (:keymap nav))]
    (when action
      (doseq [act (filter identity (action nav))]
        (a/put! (:in channels) act)))
    (assoc nav :keystate keystate)))

(defop bind [[cmd kstr action] nav more] {}
  (update-in nav [:keymap] keymap/bind (keymap/str->kseq kstr) action))

(defn create-quick-fix-fn [in persist?]
  (fn
    ([body] (a/put! in [(if persist? :fix :put) [nil :body body]]))
    ([title body]
       (doto #(a/put! in [(if persist? :fix :put) [nil %1 %2]])
         (apply [:title title])
         (apply [:body body])))))

(defop i9n-action
  [[cmd & args] n {hra :handle-returned-action :keys [widget cfg channels]}] {}
  (let [[action-map i] args
        in (:in channels)]
    (ext/custom-i9n-action
     action-map {:selected i :nav n :channels channels
                 :put (create-quick-fix-fn in false)
                 :fix (create-quick-fix-fn in :persist)
                 :handle-returned-action #(hra % i hra n)})
    n))

(defop i9n
  [[cmd & args] nav {:keys [widget cfg impl]}] {}
  (do (ext/custom-i9n (first args)
                      {:parent widget :nav nav :cfg cfg}
                      impl)
      nav))

(defop handle-returned-action
  [[cmd & args] nav {hra :handle-returned-action}] {}
  (let [[action action-args i] args]
    (hra (action (assoc action-args :state (:state nav))) i hra nav)
    nav))

;;; STEPS

(defmethod ext/custom-i9n-step :route
  [{:keys [route dispatch]} nav more]
  (secretary/add-route! (str "/" route) dispatch)
  nav)

(defmethod ext/custom-i9n-step :editable
  [{entry :entry} nav {assoc-entry :assoc-entry}]
  (assoc-entry nav entry (fn [n id] (assoc-in n [:hierarchy id :editable]
                                              true))))
