(ns org-html-slideshow.dispatch)

;; copied from https://github.com/jasonrudolph/one-rep-max/blob/master/src/lib/cljs/one/dispatch.cljs

(def ^{:doc "Stores the current reactions."}
  reactions (atom {}))

(defn react-to
  "Cause the specified reactor to be invoked whenever an event that
  satisfies `event-pred` is fired. reactor is a function that accepts
  two arguments: `event-id` and `event-data`.

  Returns the reaction.

  The reactor will continue to be invoked until one of two things
  happens:

   1. `delete-reaction` is called on this reaction.

   2. The reaction occurs `max-count` times. If `max-count` is not
      specified, the reaction will continue to be invoked until deleted.

  If `max-count` is specified, `delete-reaction` will be called
  automatically when the reaction has occurred the specified number of
  times."
  ([event-pred reactor]
   (react-to nil event-pred reactor))
  ([max-count event-pred reactor]
   (let [reaction {:max-count max-count
                   :event-pred event-pred
                   :reactor reactor}]
     (swap! reactions assoc reaction 0)
     reaction)))

(defn delete-reaction
  "Delete a reaction. After calling this function, the specified
  reaction will no longer be invoked."
  [reaction]
  (swap! reactions dissoc reaction))

(defn fire
  "Raise an event to any reactors whose event-pred returns true for
  `event-id`. The `event-id` and `event-data`, if specified, are passed to
  the reactor."
  ([event-id]
   (fire event-id nil))
  ([event-id event-data]
   (let [matching-reactions (filter (fn [[{event-pred :event-pred} run-count]]
                                      (event-pred event-id))
                                    @reactions)]
     (doseq [[reaction run-count] matching-reactions]
       (let [{:keys [max-count reactor]} reaction
             run-count (inc run-count)]
         (reactor event-id event-data)
         (if (and max-count
                  (<= max-count run-count))
           (delete-reaction reaction)
           (swap! reactions assoc reaction run-count)))))))
