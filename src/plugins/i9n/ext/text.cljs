(ns i9n.ext.text
  (:require [i9n.ext :refer [custom-i9n]]))

(defmethod custom-i9n :text
  [{:keys [content keybinds]} {:keys [parent nav cfg]}
   {:keys [render set-key-once create-text-viewer!]}]
  (let [t (create-text-viewer! parent content)
        back #(do (.detach t) (when-let [b (:back nav)] (b)) (render))]
    (set-key-once t (-> cfg :keybinds :left) back)
    (doseq [[kb trigger] (partition 2 keybinds)]
      (set-key-once t kb #(trigger
                                {:detach (fn [] (.detach t) (render))
                                 :back back})))
    (render)))
