(ns macros.core
  (:require [clojure.core.async :as async]))

;;;;; Macros for speed

(defmacro fast-+
  ([] 0)
  ([x] x)
  ([x y & more]
   (if (and (number? x) (number? y))
     `(fast-+ ~(+ x y) ~@more)
     (if (seq more)
       `(reduce + ~x [~y ~@more])
       `(+ ~x ~y)))))



(defmacro faster-+ [& args]
  (cond
    (empty? args) 0

    (= 1 (count args)) (first args)

    :else (let [nums       (filter number? args)
                static-sum (reduce + nums)
                syms       (remove number? args)]
            (if (seq syms)
              `(reduce + ~static-sum [~@syms])
              static-sum))))



;;;;; OO in lisp

(defmacro obj
  {:style/indent [1 [:defn]]}
  [init & methods]
  (let [internal (gensym)
        oset! (gensym)]
    `(let [~internal (atom ~init)
           ~oset!   (fn [p# v#] (swap! ~internal assoc p# v#) nil)]
       ~(into {}
             (map (fn [[n bindings & body]]
                    [(keyword n) `(let [~'oset! ~oset!]
                                    (fn ~bindings
                                      (let [~'this @~internal]
                                        ~@body)))]))
             methods))))

(defn oset!
  "Docstring goes here, but the magic is elsewhere."
  [o p v])

(defmacro call [o m & args]
  `((get ~o ~(keyword m)) ~@args))

(def x
  (obj {:count 3}
    (dec []
      (oset! :count (dec (:count this))))
    (inc []
      (oset! :count (inc (:count this))))
    (current []
      (:count this))))

;; Inheritance is more involved...

(defmacro derive-obj
  {:style/indent [2 [:defn]]}
  [parent init & methods]
  )

(def p
  (derive-obj x {:x 0 :y 0}
    (moveTo [x y]
      (oset! :x x)
      (oset! :y y))
    (countSpot []
      {:count (:count this)
       :spot [(:x this) (:y this)]})))

;;;;; Macros as fns

(defn fast-+-fn
  ([] 0)
  ([x] x)
  ([x y & more]
   (if (and (number? x) (number? y))
     `(fast-+-fn ~(+ x y) ~@more)
     (if (seq more)
       `(reduce + ~x [~y ~@more])
       `(+ ~x ~y)))))
