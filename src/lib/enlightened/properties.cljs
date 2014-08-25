(ns enlightened.properties)

(defn centered [props]
  (assoc props :top "center" :left "center"))

(defn halved [props]
  (assoc props :width "50%" :height "50%"))

(defn half-width [props]
  (assoc props :width "50%"))

(defn half-height [props]
  (assoc props :height "50%"))

(defn line-bordered [props]
  (assoc props :border {:type "line"}))

(defn clickable [props]
  (assoc props :mouse true))

(defn interactive [props]
  (assoc props :mouse true :keys true))

(defn interactive-vi [props]
  (assoc props :mouse true :keys true :vi true))

(defn vi [props]
  (assoc props :vi true))
