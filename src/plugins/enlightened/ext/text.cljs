(ns enlightened.ext.text
  (:require [enlightened.ext :refer [custom-i9n]]
            [enlightened.os.term :as term]
            [enlightened.os.widgets :as widgets]))

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

(defmethod custom-i9n :text [{:keys [content keybinds]} parent nav cfg]
  (let [t (create-text-viewer! parent content)
        b (or (:back nav) (constantly nil))]
    
    (term/set-key-once t (-> cfg :keybinds :left) #(do (.detach t) (b) (term/render)))
    (term/set-keys t (let [kk (into (or keybinds []) [(-> cfg :keybinds :left)
                                                      #(do (.detach t) (b))])]
                       (js/setTimeout #(.log js/console (clj->js kk)) 100)
                       kk))
    (term/render)))
