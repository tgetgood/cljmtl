(ns cljmtl.repl)

(def game-read (comp read-string read-line))

(def game-eval eval)

(def game-print println)

(defn game-repl []
  (loop []
    (-> (game-read)
        game-eval
        game-print)
    (recur)))
