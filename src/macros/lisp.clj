(ns macros.lisp)

;;;;; Implement let as a macro

(defmacro my-let
  {:style/indent [1]}
  [bindings & body]
  (let [pairs (partition 2 bindings)
        syms  (map first pairs)
        vals  (map second pairs)]
    `((fn [~@syms]
        (do
          ~@body))
      ~@vals)))

(defn nested-let [[p & pairs]  body]
  (if p
    `((fn [~(first p)]
        ~(nested-let pairs body))
      ~(second p))
    body))

(defmacro my-prebinding-let
  {:style/indent [1]}
  [bindings & body]
  (nested-let (partition 2 bindings) `(do ~@body)))

;;;; cond

(defn nested-if [[[pred branch] & more]]
  `(if ~pred
     ~branch
     ~(when (seq more) (nested-if more))))

(defmacro my-cond [& args]
  (assert (even? (count args)) "Cond takes an even number of forms.")
  (let [pairs (partition 2 args)]
    (nested-if pairs)))
