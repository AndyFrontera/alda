(ns alda.util
  (:import [java.io File])
  (:require [clojure.string :as str]))

(defmacro pdoseq
  "A fairly efficient hybrid of `doseq` and `pmap`"
  [binding & body]
  `(doseq ~binding
     (future ~@body)))

(defn strip-nil-values
  "Strip `nil` values from a map."
  [hsh]
  (into (empty hsh) (remove (comp nil? last)) hsh))

(defn parse-str-opts
  "Transform string based keyword arguments into a regular map, eg.
   IN:  \"start 0:20 finish :third-movement some-junk-at-end\"
   OUT: {:start  \"0:20\"
         :finish \":third-movement\"}"
  [opts-str]
  (let [pairs (partition 2 (str/split opts-str #"\s"))]
    (into {} (map (fn [[k v]] [(keyword k) v])) pairs)))

(defn parse-time
  "Convert a human readable duration into milliseconds, eg. \"02:31\" => 151 000"
  [time-str]
  (let [[s m h] (as-> (str/split time-str #":") x
                      (reverse x)
                      (map #(Double/parseDouble %) x)
                      (concat x [0 0 0]))]
    (* (+ (* 60 (+ (* 60 h) m)) s) 1000)))

(defn parse-position
  "Convert a string denoting a position in a song into the appropriate type.
   For explicit timepoints this is a double denoting milliseconds, and for
   markers this is a keyword."
  [position-str]
  (when position-str
    (if (.startsWith position-str ":")
      (keyword (subs position-str 1))
      (parse-time position-str))))

(defn check-for
  "Checks to see if a given file already exists. If it does, prompts the user
   whether he/she wants to overwrite the file. If he/she doesn't, then prompts
   the user to choose a new filename and calls itself to check the new file, etc.
   Returns a filename that does not exist, or does exist and the user is OK with
   overwriting it."
  [filename]
  (letfn [(prompt []
            (print "> ") (flush) (read-line))
          (overwrite-dialog []
            (println
              (format "File \"%s\" already exists. Overwrite? (y/n)" filename))
            (let [response (prompt)]
              (cond
                (re-find #"(?i)y(es)?" response)
                filename

                (re-find #"(?i)no?" response)
                (do
                  (println "Please specify a different filename.")
                  (check-for (prompt)))

               :else
               (do
                 (println "Answer the question, sir.")
                 (recur)))))]
    (cond
      (.isFile (File. filename))
      (overwrite-dialog)

      (.isDirectory (File. filename))
      (do
        (printf "\"%s\" is a directory. Please specify a filename.\n" filename)
        (recur (prompt)))

      :else
      filename)))
