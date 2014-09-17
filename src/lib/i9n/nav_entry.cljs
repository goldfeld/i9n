(ns i9n.nav-entry
  "Pure, testable helpers for i9n.navigation"
  (:require [i9n.more :refer [index-of splice replace-at-indexes]]
            [i9n.step :refer [i9n-step]]))

(defn add-to-hierarchy
  ([nav nav-entries] (add-to-hierarchy nav nav-entries nil))
  ([nav nav-entries update-in-entry]
     (reduce (fn [n entry]
               (condp apply [entry]
                 vector?
                 (let [[id title & args] entry
                       has-trigger (= 2 (count args))
                       n' (if has-trigger
                            (assoc-in n [:hierarchy id :trigger] (first args))
                            n)]
                   (-> (if update-in-entry (update-in-entry n' id) n')
                       (assoc-in [:hierarchy id :data]
                                 (conj [title] ((if has-trigger second first)
                                                args)))))
                 map? (if (contains? entry :i9n-step)
                        (i9n-step entry n {}))
                 n))
             nav nav-entries)))

(defn create-nav [root-item nav-entries]
  (add-to-hierarchy {:hierarchy {:root {:data root-item}}}
                    nav-entries))

(defn fill-body
  "Prepares an entry's body for a fix by filling with nils as
  needed. If body is not a vector, return a vector full of nil
  elements."
  [body place fix-length]
  (if (vector? body)
    (let [elements-to-add (- (+ place fix-length) (count body))]
      (if (> elements-to-add 0)
        (into body (repeat elements-to-add nil))
        body))
    (vec (repeat (+ place fix-length) nil))))

(defn noop-fix [nav _ _] nav)

(defn fix-at
  [place fix replace? nav _ body-path]
  (update-in nav body-path
             #(let [fix-length (if replace? (count fix) 0)]
                (vec (apply splice (fill-body % place fix-length)
                            place fix-length fix)))))

(defn create-fix-search
  "Returns a function applying a fix within some integer distance
  (determined by offset-fn) of the first item within search-items
  found in the body of the nav-entry designated by body-path. See
  create-fix."
  [search-items offset-fn fix replace?]
  (fn [nav _ body-path]
    (if-let [i (apply index-of (get-in nav body-path) search-items)]
      (fix-at (offset-fn i) fix replace? nav _ body-path)
      nav)))

(def x2 (partial * 2))

(defn create-replacements-fix
  "Returns a function applying the fix vector over a range starting at
  i, and skipping every other item. This allows a fix consisting
  solely of either labels or actions to be applied over an entry's
  body."
  [i fix]
  (fn [nav _ body-path]
    (let [flength (x2 (count fix))]
      (replace-at-indexes (fill-body (get nav body-path) i flength)
                          (range i (+ i flength 2))
                          fix))))

(defn create-replacements-fix-search
  [search-items offset-fn fix]
  (fn [nav _ body-path]
    (if-let [i (apply index-of (get-in nav body-path) search-items)]
      ((create-replacements-fix (offset-fn i) fix) nav _ body-path)
      nav)))

(def current-label #(if (even? %) % (dec %)))
(def current-action #(if (odd? %) % (inc %)))
(def next-label #(inc (if (even? %) (inc %) %)))

(defn create-fix
  "Returns a function which applies a surgical fix to a nav-entry or
  hierarchy entry, where the distinction between the two is made by
  supplying the proper paths to title & body within the entry
  vector. It also may target either the title or the body, making use
  of such paths. When the target is a body and it is not a vector,
  assume that whatever originates the fix has made use of the
  non-vector body first, if applicable, and so overwrite it with a
  sparse vector (containing the fix and filled with nils.)"
  [place fix]
  (condp apply [place]
    integer? (if (>= place 0) (partial fix-at place fix :replace) noop-fix)
    keyword?
    (case place
      :title (fn [nav tpath _] (assoc-in nav tpath (first fix)))
      :body (fn [nav _ bpath] (assoc-in nav bpath (first fix)))
      :append (fn [nav _ bpath]
                (update-in nav bpath #(into (or (and (vector? %) %) []) fix)))
      :prepend (fn [nav _ bpath]
                 (update-in nav bpath #(into (vec fix)
                                             (or (and (vector? %) %) []))))
      :last (fn [nav _ bpath]
              (update-in nav bpath
                         #(fix-at (-> % count dec dec) fix true nav _ bpath)))
      :last-action (fn [nav _ bpath]
                     (update-in nav bpath
                                #(fix-at (dec (count %)) fix true nav _ bpath)))
      :pop (fn [nav _ bpath]
             (update-in nav bpath #(if (and (vector? %) (seq %))
                                     (pop (if (odd? (count %)) % (pop %)))
                                     [])))
      noop-fix)
    vector?
    (let [[cmd & [arg & more :as args]] place]
      (case cmd
        :insert (partial fix-at (x2 arg) fix false)
        :insert-after (create-fix-search args next-label fix false)
        :insert-before (create-fix-search args current-label fix false)
        :action (create-replacements-fix (inc (x2 arg)) fix)
        :label (create-replacements-fix (x2 arg) fix)
        :action-find (create-replacements-fix-search args current-action fix)
        :label-find (create-replacements-fix-search args current-label fix)
        :from (create-fix-search args current-label fix :replace)
        :after (create-fix-search more #(+ % (x2 arg)) fix :replace)
        :before (create-fix-search more #(- % (x2 arg)) fix :replace)
        :shrink (fn [nav _ bpath]
                  (update-in nav bpath #(splice % (x2 (second args))
                                                (x2 arg))))
        :remove (fn [nav _ bpath]
                  (when-let [i (apply index-of (get-in nav bpath) more)]
                    (update-in nav bpath #(splice % (current-label i)
                                                  (x2 arg)))))
        noop-fix))
    noop-fix))
