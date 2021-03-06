(ns i9n.keymap
  (:require [clojure.string :as strng]))

(defn handle-key [k keystate km]
  "Returns a 2-element vector containing an action and a new keystate;
  an action is the bound value in a keymap km, and the keystate is the
  current value of a state machine keeping track of keys pressed
  sequentially to trigger a binding."
  (if (= k "escape")
    [nil []]
    (let [ks (conj keystate k)
          action (get km ks)]
      (condp apply [action]
        nil? [nil []]
        true? [nil ks]
        [action []]))))

(defn str-chars [s]
  (rest (strng/split s #"")))

(def uppercase
  (set (str-chars "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))

(def special
  #{"enter" "escape"
    "left" "right" "down" "up"})

(defn str->kseq [s]
  (let [result
        (-> (fn [{:keys [escape-seq escaping?] :as res} c]
              (cond
               escaping? (let [esc-seq (strng/lower-case
                                        (str escape-seq c))]
                           (if (or (some special [esc-seq])
                                   (re-find #"^[csm]-.$" esc-seq))
                             (-> (update-in res [:kseq] conj esc-seq)
                                 (assoc :escaping? false
                                        :escape-seq ""))
                             (update-in res [:escape-seq] str c)))
               (= "\\" c) (-> (assoc res :escaping? true))
               :else (update-in res [:kseq] conj
                                (if (some uppercase [c])
                                  (str "s-" (strng/lower-case c))
                                  c))))
            (reduce {:kseq [] :escape-seq "" :escaping? false}
                    (str-chars s)))]
    (if (:escaping? result)
      (throw (js/Error. (str "Invalid escape sequence in binding '" s "'")))
      (:kseq result))))

(def bound? (every-pred identity (complement true?)))

(defn bind
  ([km kseq action] (bind km kseq action kseq))
  ([km kseq action prefix]
     (if (seq prefix)
       (if (bound? (get km prefix))
         (throw (js/Error. (str "Can't bind key sequence '" (apply str kseq)
                                "', '" (apply str prefix) "' already bound.")))
         (bind km kseq action (butlast prefix)))
       (apply assoc km kseq action
              (interleave (iterate butlast (butlast kseq))
                          (repeat (dec (count kseq)) true))))))

(defn make-keymap [action-map action-defs]
  (reduce-kv (fn [km kstr action]
               (bind km (str->kseq kstr)
                     (condp apply [action]
                       keyword? (or (:fn (get action-defs action))
                                    (fn [n] [[action]]))
                       map? (:fn action)
                       (fn [n] []))))
             {} action-map))

(def action-defs
  {:down {:type :move :fn (fn [n] [[:select + 1]])}
   :up {:type :move :fn (fn [n] [[:select - 1]])}
   :top {:type :move :fn (fn [n] [[:select 0]])}
   :bottom {:type :move :fn (fn [n] [[:select (:last n)]])}
   :back {:type :hop :fn (fn [n] [[:back]])}
   :pick {:type :control :fn (fn [n] [[:pick]])}
   :undo {:type :control :fn (fn [n] [[:undo]])}
   :remove {:fn (fn [n] [[:user-fix [nil :remove]]])}})

(def vi-actions
  {"j" :down, "\\DOWN" :down, "k" :up, "\\UP" :up
   "gg" :top, "G" :bottom
   "h" :back, "\\LEFT" :back
   "l" :pick, "\\RIGHT" :pick, "\\ENTER" :pick
   "dd" :remove
   "dj" {:type :move :fn (fn [n] [[:user-fix [nil [:remove 2]]]])}
   "u" :undo
   "H" :history})

(def vi (make-keymap vi-actions action-defs))
