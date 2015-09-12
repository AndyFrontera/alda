(ns alda.now)

(require '[alda.sound  :as sound]
         '[alda.cli]
         '[clojure.set :as set])

; sets log level to TIMBRE_LEVEL (if set) or :warn
(alda.cli/set-timbre-level!)

(require '[alda.lisp :as lisp])

(def set-up! sound/set-up!)

(defn- normalize-events
  "Remove events outside of [start,end), if they are present.
   Shift the remaining events such that the earliest event occurs at 0.
   Returns a lazy seq of events."
  [events]
  (let [earliest (apply min (map :offset events))]
    (alda.sound/shift-events events earliest nil)))

(defn play-new-events!
  [events]
  (let [events (lisp/event-set {:start
                                {:offset (lisp/->AbsoluteOffset 0)
                                 :events events}})
        shifted (normalize-events events)
        one-off-score (assoc (lisp/score-map)
                             :events shifted)]
    (sound/play! one-off-score)))

(defmacro play!
  "Evaluates some alda.lisp code and plays only the new events."
  [& body]
  `(let [old-score# (lisp/score-map)
         new-score# (do ~@body (lisp/score-map))
         new-events# (set/difference
                       (:events new-score#)
                       (:events old-score#))]
     (play-new-events! new-events#)))

(defn refresh!
  "Clears all events and resets the current-offset of each instrument to 0.

   Useful for playing a new set of notes with multiple instrument parts,
   ensuring that both parts start at the same time, regardless of any prior
   difference in current-offset between the instrument parts."
  []
  (alter-var-root #'alda.lisp/*instruments*
    #(into {}
       (map (fn [[instrument attrs]]
              [instrument
               (assoc attrs :current-offset (lisp/->AbsoluteOffset 0)
                            :last-offset (lisp/->AbsoluteOffset 0))])
            %)))
  (alter-var-root #'alda.lisp/*events*
    (constantly {:start {:offset (lisp/->AbsoluteOffset 0), :events []}})))
