(ns alda.macros
  (:refer-clojure :exclude (slurp))
  (:require [clojure.java.io :as io]))

(defmacro slurp 
  "Slurps a file relative to the working directory (this will not work for
   resource files)."
  [file]
  (clojure.core/slurp file))

(defmacro slurp-file 
  "Slurps a file that is a resource of the alda project."
  [file]
  (clojure.core/slurp (io/resource file)))
