(ns tetris.core
  (:require tetris.display)
  (:require [clojure.contrib.command-line :as ccl]))

(defn -main [& args]
  (ccl/with-command-line args
                         "Tetris"
                         [[level "Starting level of the game" "1"] extras]
                         (println "Starting tetris on level" level)
                         (tetris.display/tetris-swing (Integer/parseInt level))))

