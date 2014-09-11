(ns enlightened.ext.text)

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

(defmethod i9n :text [{:keys [content keybinds]} parent nav cfg]
  (let [t (create-text-viewer! parent content)
        b (or (:back nav) (constantly nil))]
    (term/set-keys t (into (or keybinds []) [(-> cfg :keybinds :left)
                                             #(do (.detach t) (b))]))))
