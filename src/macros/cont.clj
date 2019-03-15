(ns macros.cont)

;;;;; Macros for continuations

(def ^:dynamic *cont* identity)

(defmacro =fn [bindings & body]
  `(fn ~(into `[~'*cont*] bindings) ~@body))

(defmacro =defn
  {:style/indent :defn}
  [n params & body]
  (let [f (symbol (str "=" (name n)))]
    `(do
       (defn ~f [~'*cont* ~@params]
         ~@body)
       (defmacro ~n ~params
         (~f ~'*cont* ~@params)))))

(defmacro =bind
  {:style/indent 2}
  [params expr & body]
  `(let [~'*cont* (fn ~params ~@body)]
     ~expr))

(defmacro =return [& retvals]
  `(~'*cont* ~@retvals))

(defmacro =apply [f & args]
  `(f ~'*cont* ~@args))

;;; Example

#_(defmacro await
  {:style/indent 1}
  [[binding expr] & body]
  `(=bind [~binding] (do ~@body) ~expr))

(defn ch [] (atom {:vals [] :listeners []}))

(defn run-ch! [ch]
  (let [{:keys [vals listeners]} @ch]
    (loop [[v & vals] vals
           [l & listeners] listeners]
      (if (and v l)
        (l v)
        (swap! ch assoc :vals (into [] vals)
               :listeners (into [] listeners))))))

(=defn put! [v]
  (=fn [] v))

(=defn await [ch f]
  (=bind [x] ch (f (x))))
