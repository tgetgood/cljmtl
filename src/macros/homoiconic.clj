(ns macros.homoiconic)

(def form '(fn [x] (+ x 5)))

(def rp1 (comp str read-string))

(def rep1 (comp str eval read-string))

(def read-macroexpand-print (comp str macroexpand read-string))

;;;;; Macro failings

(defmacro broken-now [g]
  `{:a (~f 7)
    :b ~f})
