(ns kiran.core)

(defmacro suppress
  "Suppresses any errors thrown.

    (suppress (error \"Error\")) ;=> nil

    (suppress (error \"Error\") :error) ;=> :error
  "
  ([body]
     `(try ~body (catch Throwable ~'t)))
  ([body catch-val]
     `(try ~body (catch Throwable ~'t
                   (cond (fn? ~catch-val)
                         (~catch-val ~'t)
                         :else ~catch-val)))))

(defn atom? [obj] (instance? clojure.lang.Atom obj))
(defn aref? [obj] (instance? clojure.lang.Ref obj))

(defn set-value!
  "Change the value contained within a ref or atom.

    @(set-value! (atom 0) 1)
    ;=> 1

    @(set-value! (ref 0) 1)
    ;=> 1
  "
  [rf obj]
  (cond (atom? rf) (reset! rf obj)
        (aref? rf) (dosync (ref-set rf obj)))
  rf)

(defn iref?
"Returns `true` if `x` is of type `clojure.lang.IRef`.

  (iref? (atom 0)) ;=> true
"
[obj]  (instance? clojure.lang.IRef obj))

(def nothing ::nothing)

(defn nothing? [x]
  (= x nothing))

(defn straight-through [& [x]] x)

(defn cell-state
  ([label content]
     {:label       (atom label)
      :content     (ref content)
      :propagators (atom #{})}))

(defn propagator-state
  ([label in-cells out-cell tf tdamp concurrent]
     {:label (atom label)
      :in-cells (atom in-cells)
      :out-cell (atom out-cell)
      :tf (atom tf)
      :tdamp (atom tdamp)
      :concurrent? (atom concurrent)}))

(defprotocol PutProtocol
  (put! [mut k val]))

(defprotocol PropagatorProtocol
  (propagate! [pg]))

(deftype Propagator [state]
  PropagatorProtocol
  (propagate! [pg]
    (let [ins     (map deref (:in-cells pg))
          tf      (:tf pg)
          tdamp   (:tdamp pg)
          outcell (:out-cell pg)
          out     (if-not (some nothing? ins)
                    (suppress (apply tf ins) nothing)
                    nothing)]
      (if-not (or (nothing? out)
                  (suppress (tdamp @outcell out)))
        (if (:concurrent? pg)
          (future (outcell out))
          (outcell out)))))

  clojure.lang.ILookup
  (valAt [pg k] (if-let [res (get state k)]
                    (if (iref? res)
                      @res res)))
  (valAt [pg k not-found] (or (get pg k) not-found))

  PutProtocol
  (put! [pg k val]
    (if-let [res (get state k)]
      (if (iref? res)
       (set-value! res val)))
    pg))

(defprotocol CellProtocol
  (register-propagator [cell pg])
  (deregister-propagator [cell pg])
  (notify-propagators [cell]))

(deftype Cell [state]
  CellProtocol
  (register-propagator [cell pg]
    (swap! (:propagators state) conj pg)
    cell)
  (deregister-propagator [cell pg]
    (swap! (:propagators state) disj pg)
    cell)
  (notify-propagators [cell]
    (doseq [p (:propagators cell)]
      (propagate! p)))

  PutProtocol
  (put! [cell k val]
    (if-let [res (get state k)]
      (if (iref? res)
        (set-value! res val)))
    cell)

  clojure.lang.ILookup
  (valAt [cell k] (if-let [res (get state k)]
                    (if (iref? res)
                      @res res)))
  (valAt [cell k not-found] (or (get cell k) not-found))

  clojure.lang.IDeref
  (deref [cell] (:content cell))

  clojure.lang.IFn
  (invoke [cell content]
    (put! cell :content content)
    (notify-propagators cell)
    cell)

  clojure.lang.IRef
  (setValidator [cell vf] (.setValidator (:content state) vf))
  (getValidator [cell] (.getValidator (:content state)))
  (getWatches [cell] (.getWatches (:content state)))
  (addWatch [cell key callback] (add-watch (:content state) key callback))
  (removeWatch [cell key] (remove-watch (:content state) key)))

(defn cell
  ([] (cell nothing))
  ([content] (Cell. (cell-state nil content)))
  ([label content] (Cell. (cell-state label content))))

(defn propagator
  ([label] (propagator label straight-through))
  ([label tf] (propagator label tf =))
  ([label tf tdamp] (propagator label [] nil tf tdamp))
  ([in-cells out-cell tf tdamp]
     (propagator nil [] nil tf tdamp))
  ([label in-cells out-cell tf tdamp]
     (Propagator. (propagator-state label in-cells out-cell tf tdamp true))))

(defn connect
  ([sources sink] (connect nil sources sink straight-through))
  ([sources sink tf] (connect nil sources sink tf =))
  ([label sources sink tf] (connect label sources sink tf =))
  ([label sources sink tf tdamp]
     (let [pg (propagator label sources sink tf tdamp)]
       (doseq [s sources]
         (register-propagator s pg))
       pg)))

(defn disconnect [pg]
  (let [sources (:in-cells pg)]
    (doseq [s sources]
      (deregister-propagator s pg))
    (put! pg :in-cells [])
    (put! pg :out-cell nil)))

(defn network [inputs output f])

(defn- label-or-hash [obj]
  [(or (:label obj) (.hashCode obj))])

(defmethod print-method
  Propagator
  [pg w]
  (print-method
   (let [hash (.hashCode pg)]
     (format "<P@%s %s => %s>"
             hash
             (->> (map label-or-hash (:in-cells pg))
                  (cons (or (:label pg) 'fn)) (apply list) str)
             (label-or-hash (:out-cell pg))))
   w))

(defmethod print-method
  Cell
  [cell w]
  (print-method
   (let [hash (.hashCode cell)]
     (format "<Cell@%s %s>"
             hash @cell))
   w))
