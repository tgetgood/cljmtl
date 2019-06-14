(ns cljmtl.core
  (:require [cljmtl.repl :as repl]))

(defn start-game! []
  (repl/game-repl))
