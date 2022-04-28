(ns lazy-map.core
  "Maps that only realize their values when the value is accessed."
  (:require [clojure.pprint :as pp]
            [clojure.walk :as walk]
            [clojure.set :as sets])
  (:import java.io.Writer
           (java.util Map RandomAccess Map$Entry)
           (clojure.lang Seqable IPersistentMap
                         IPersistentCollection IMapIterable
                         ILookup IKVReduce IObj IFn Associative
                         Sequential Reversible IPersistentVector
                         IPersistentStack Indexed IMapEntry IHashEq
                         MapEntry IPersistentSet IPersistentList
                         APersistentMap$KeySeq PersistentList MapEquivalence
                         APersistentMap ISeq)
           (java.io Serializable))
  (:refer-clojure :exclude (merge select-keys)))

(set! *warn-on-reflection* true)

;;;; Utility functions

(defmacro extend-print
  "Convenience macro for overriding the string representation of a
  class. Note that you also need to override `toString` in order to
  customize the return value of `str` on your object, and you need to
  create your own dispatch function to customize the pretty-printed
  representation."
  {:private true}
  [class str-fn]
  `(do
     ;; for serialization
     (defmethod print-dup ~class
       [obj# ^Writer writer#]
       (.write writer# ^String (~str-fn obj#)))
     ;; for reader-friendly printing
     (defmethod print-method ~class
       [obj# ^Writer writer#]
       (.write writer# ^String (~str-fn obj#)))))

(defn map-entry
  "Creates a map entry (as returned by calling seq on a map) with the
  given key and value."
  [k v]
  (MapEntry/create k v))

;;;; PlaceholderText type

(defrecord PlaceholderText [text])

(extend-print PlaceholderText :text)

;;;; LazyMapEntry type

(deftype LazyMapEntry [key_ val_]

  Associative
  (containsKey [this k]
    (boolean (#{0 1} k)))
  (entryAt [this k]
    (cond
      (= k 0) (map-entry 0 key_)
      (= k 1) (LazyMapEntry. 1 val_)
      :else nil))
  (assoc [this k v]
    (cond
      (= k 0) (LazyMapEntry. v val_)
      (= k 1) (LazyMapEntry. key_ v)
      (= k 2) (vector k (force val_) v)
      :else (throw (IndexOutOfBoundsException.))))

  IFn
  (invoke [this k]
    (.valAt this k))

  IHashEq
  (hasheq [this]
    (.hasheq
      ^IHashEq
      (vector key_ (force val_))))

  ILookup
  (valAt [this k]
    (cond
      (= k 0) key_
      (= k 1) (force val_)
      :else nil))
  (valAt [this k not-found]
    (cond
      (= k 0) key_
      (= k 1) (force val_)
      :else not-found))

  IMapEntry
  (key [this] key_)
  (val [this] (force val_))

  Indexed
  (nth [this i]
    (cond
      (= i 0) key_
      (= i 1) (force val_)
      (integer? i) (throw (IndexOutOfBoundsException.))
      :else (throw (IllegalArgumentException. "Key must be integer")))
    (.valAt this i))
  (nth [this i not-found]
    (try
      (.nth this i)
      (catch Exception _ not-found)))

  IPersistentCollection
  (count [this] 2)
  (empty [this] nil)
  (equiv [this o]
    (.equiv
      [key_ (force val_)]
      o))

  IPersistentStack
  (peek [this] (force val_))
  (pop [this] [key_])

  IPersistentVector
  (assocN [this i v]
    (.assocN [key_ (force val_)] i v))
  (cons [this o]
    (.cons [key_ (force val_)] o))

  Reversible
  (rseq [this] (lazy-seq (list (force val_) key_)))

  Seqable
  (seq [this]
    (cons key_ (lazy-seq (list (force val_)))))

  Sequential

  Serializable

  Comparable
  (compareTo [this o]
    (.compareTo
      ^Comparable
      (vector key_ (force val_))
      o))

  Iterable
  (iterator [this]
    (.iterator
      ^Iterable
      (.seq this)))

  Object
  (toString [this]
    (str [key_ (if (and (delay? val_)
                        (not (realized? val_)))
                 (->PlaceholderText "<unrealized>")
                 (force val_))]))

  Map$Entry
  (getKey [this] key_)
  (getValue [this] (force val_))

  RandomAccess)

(defn lazy-map-entry
  "Construct a lazy map entry with the given key and value. If you
  want to take advantage of the laziness, the value should be a
  delay."
  [k v]
  (LazyMapEntry. k v))

(extend-print LazyMapEntry #(.toString ^LazyMapEntry %))

;;;; LazyMap type

;; We need to use this function in .toString so it needs to be defined
;; before the deftype. But the definition of this function needs a
;; ^LazyMap type hint, so the definition can't come until after the
;; deftype.
(declare freeze-map)

(deftype LazyMap [^IPersistentMap contents]

  Map
  (size [this]
    (count contents))

  (isEmpty [this]
    (empty? contents))

  (containsValue [this value]
    (let [entries
          (reduce-kv
            (fn [agg k v]
              (if (or (not (delay? v)) (realized? v))
                (update agg true conj (force v))
                (update agg false conj v)))
            {true #{} false []}
            contents)]
      ; check realized values first so we don't unnecessarily realize more
      (or (contains? (get entries true) value)
          (loop [[item :as items] (get entries false [])]
            (if (empty? items)
              false
              (if (= (force item) value)
                true
                (recur (rest items))))))))

  (get [this key]
    (.valAt this key))

  (keySet [this]
    (set (keys contents)))

  (values [this]
    (map force (vals contents)))

  (entrySet [this]
    (reduce-kv #(conj %1 (lazy-map-entry %2 %3)) #{} contents))

  Associative
  (containsKey [this k]
    (.containsKey contents k))

  (entryAt [this k]
    (lazy-map-entry k (.valAt contents k)))

  IFn
  (invoke [this k]
    (.valAt this k))
  (invoke [this k not-found]
    (.valAt this k not-found))

  IKVReduce
  (kvreduce [this f init]
    (reduce-kv f init (into {} this)))

  ILookup
  (valAt [this k]
    (force (.valAt contents k)))

  (valAt [this k not-found]
    (let [sentinel (Object.)
          value    (.valAt contents k sentinel)]
      (if (identical? value sentinel) not-found (force value))))

  IMapIterable
  (keyIterator [this]
    (if-some [key-seq ^APersistentMap$KeySeq (keys contents)]
      (.iterator key-seq)
      (.iterator PersistentList/EMPTY)))

  (valIterator [this]
    (.iterator
      ;; Using the higher-arity form of map prevents chunking.
      ^Iterable
      (map (fn [[k v] _]
             (force v))
           contents
           (repeat nil))))

  IObj
  (meta [this]
    (:metadata this))

  (withMeta [this metadata]
    (assoc this :metadata metadata))

  IPersistentCollection
  (count [this]
    (.count contents))
  (empty [this]
    (LazyMap. (.empty contents)))
  (cons [this o]
    (LazyMap. (.cons contents o)))
  (equiv [this o]
    (.equiv ^IPersistentMap (into {} this) o))

  IPersistentMap
  (assoc [this key val]
    (LazyMap. (.assoc contents key val)))

  (without [this key]
    (LazyMap. (.without contents key)))

  Seqable
  (seq [this]
    ;; Using the higher-arity form of map prevents chunking.
    (seq
      (map (fn [[k v] _]
             (lazy-map-entry k v))
           contents
           (repeat nil))))

  Iterable
  (iterator [this]
    (.iterator ^Iterable (.seq this)))

  MapEquivalence

  Object
  (equals [this that]
    (APersistentMap/mapEquals this that))

  (toString [this]
    (str (freeze-map (->PlaceholderText "<unrealized>") this))))

(alter-meta!
  #'->LazyMap assoc :doc
  "Turn a regular map into a lazy map. Any values that are delays
  are interpreted as values that have yet to be realized.")

(extend-print LazyMap #(.toString ^LazyMap %))

(defmethod pp/simple-dispatch lazy_map.core.LazyMap [obj]
  (pp/simple-dispatch (freeze-map (->PlaceholderText "<unrealized>") obj)))

(defmethod pp/simple-dispatch lazy_map.core.LazyMapEntry [^lazy_map.core.LazyMapEntry obj]
  (pp/simple-dispatch
    (let [raw-value (.val_ obj)]
      (map-entry (key obj) (if (and (delay? raw-value)
                                    (not (realized? raw-value)))
                             (->PlaceholderText "<unrealized>")
                             (force raw-value))))))

(defmethod pp/simple-dispatch lazy_map.core.PlaceholderText [obj]
  (pr obj))

(defn backport-seqable?
  "Equivalent to clojure 1.9's seqable?"
  [x]
  (or (instance? ISeq x)
      (instance? Seqable x)
      (nil? x)
      (instance? Iterable x)
      (.isArray (class x))
      (instance? CharSequence x)
      (instance? Map x)))

(defn dynamic-form? [x]
  (letfn [(quoted? [x]
            (and (seq? x) (= 'quote (first x))))
          (branch? [x]
            (and (not (string? x)) (backport-seqable? x) (not (quoted? x))))
          (dynamic? [x]
            (or (var? x)
                (symbol? x)
                (and (seq? x) (not (quoted? x)) (not (empty? x)))))]
    (reduce (fn [nf x] (if (dynamic? x) (reduced true) nf)) false (tree-seq branch? seq x))))

(defprotocol LazyRewrite
  (rewrite [form]
    "Rewrites form into a lazy construct."))

(extend-protocol LazyRewrite
  nil
  (rewrite [form]
    nil)
  Object
  (rewrite [form]
    form)
  MapEntry
  (rewrite [[k v]]
    [(rewrite k)
     (let [v' (rewrite v)]
       (if (dynamic-form? v')
         (if (and (seq? v') (= 'lazy-map.core/->LazyMap (first v')))
           v'
           (list `delay v'))
         v'))])
  IPersistentList
  (rewrite [form]
    (map rewrite form))
  IPersistentSet
  (rewrite [form]
    (into #{} (map rewrite) form))
  IPersistentVector
  (rewrite [form]
    (into [] (map rewrite) form))
  Map
  (rewrite [form]
    `(->LazyMap ~(apply hash-map (mapcat rewrite form)))))

(defn- reduce-kv* [f init coll]
  (if (satisfies? clojure.core.protocols/IKVReduce coll)
    (reduce-kv f init coll)
    (reduce (fn [agg [k v]] (f agg k v)) init coll)))



;;; BEGIN PUBLIC API

(defn lazy-map?
  "Is m a lazy map?"
  [m]
  (instance? lazy_map.core.LazyMap m))


(defn lazy-map-entry?
  "Is e a lazy map entry?"
  [e]
  (instance? lazy_map.core.LazyMapEntry e))


(defn realized-at?
  "Returns false if accessing k in m would cause a delay to execute."
  [m k]
  (if-some [v (find m k)]
    (if (lazy-map-entry? v)
      (or (not (delay? (.val_ ^lazy_map.core.LazyMapEntry v)))
          (realized? (.val_ ^lazy_map.core.LazyMapEntry v)))
      true)
    true))


(defn lazy-map
  "Constructs a lazy map from a map that may contain delays for values."
  ([] (->LazyMap {}))
  ([m]
   ((fn lazy-one [m]
      (if (lazy-map? m)
        m
        (->LazyMap
          (reduce-kv
            (fn [agg k v]
              (if (map? v)
                (assoc agg k (lazy-one v))
                (assoc agg k v)))
            {}
            (or m {})))))
    m)))


(defmacro literal->lazy-map
  "Constructs a lazy map from a literal map. None of the values are
  evaluated until they are accessed from the map. Recursively converts
  any nested inline maps as well."
  [form]
  (rewrite form))


(defn force-map
  "Realizes all the values in a lazy map, returning a regular map."
  [map]
  (walk/postwalk
    (fn [form]
      (if (lazy-map? form)
        (into {} form)
        form))
    map))


(defn unwrap
  "If m is a lazy map, it returns it in unwrapped form. Else, returns m."
  [m]
  (if (lazy-map? m)
    (.-contents ^lazy_map.core.LazyMap m)
    m))


(defn fmap
  "Applies a function to a map. If it's a lazy map it unwraps it,
   applies the function, then wraps it up again."
  [m f & args]
  (if (lazy-map? m)
    (lazy-map (apply f (unwrap m) args))
    (apply f m args)))


(defn select-keys
  "Same as clojure.core/select-keys but doesn't realize lazy values."
  [m keyseq]
  (fmap m clojure.core/select-keys keyseq))


(defn filter-keys
  "Filters the map according to a predicate of the keys."
  [pred m]
  (fmap m (fn [m'] (into {} (filter (comp pred key)) m'))))


(defn remove-keys
  "Removes the map entries that match a predicate of the keys."
  [pred m]
  (filter-keys (complement pred) m))


(defn map-keys
  "Applies f to every key in m. Preserves laziness of values in m."
  [f m]
  (fmap m (fn [m']
            (persistent!
              (reduce-kv*
                (fn [m'' k v] (assoc! m'' (f k) v))
                (transient (empty m'))
                m')))))


(defn map-vals
  "Stages applying f to every val in m without realizing any."
  [f m]
  (fmap m (fn [m']
            (persistent!
              (reduce-kv*
                (fn [m'' k v]
                  (assoc! m'' k (delay (f (force v)))))
                (transient (empty m'))
                m')))))


(defn freeze-map
  "Replace all the unrealized values in a lazy map with placeholders,
  returning a regular map. No matter what is done to the returned map,
  the values in the original map will not be forced. v can be an
  object to use for all the values or a function of the key."
  [val map]
  (let [val (if (fn? val) val (constantly val))]
    ((fn freeze-one [m]
       (reduce-kv
         (fn [m k v]
           (let [new-v (if (and (delay? v) (not (realized? v))) (val k) (force v))]
             (assoc m k (if (lazy-map? new-v) (freeze-one new-v) new-v))))
         {}
         (if (lazy-map? m) (.contents ^LazyMap m) m)))
     map)))


(defn merge
  "Merges maps. Preserves laziness of unrealized values in lazy maps."
  [& maps]
  (letfn [(value [m1 m2 k]
            (case [(contains? m1 k) (contains? m2 k)]
              ([true true] [false true])
              (if (realized-at? m2 k)
                (get m2 k)
                (delay (get m2 k)))

              [true false]
              (if (realized-at? m1 k)
                (get m1 k)
                (delay (get m1 k)))

              [false false] nil))

          (inner-merge [m1 m2]
            (->> (sets/union (keys m1) (keys m2))
                 (into {} (map (fn [k] [k (value m1 m2 k)])))
                 (->LazyMap)))]

    (reduce inner-merge (->LazyMap {}) (remove empty? maps))))


(defn deep-merge
  "Deep merges maps. Preserves laziness of unrealized values in lazy maps."
  [& maps]
  (letfn [(value [m1 m2 k]

            (case [(contains? m1 k) (contains? m2 k)]

              ; exists in both maps
              [true true]

              (case [(realized-at? m1 k) (realized-at? m2 k)]

                ; is already realized in both maps
                ; just access and merge if necessary.
                [true true]
                (let [m2v (get m2 k)]
                  (if (map? m2v)
                    (let [m1v (get m1 k)]
                      (if (map? m1v)
                        (inner-merge m1v m2v)
                        m2v))
                    m2v))

                ; k is realized in m2 but not in m1. if entry in m2 is not a recursive
                ; deep-merge candidate then we can just return eagerly. otherwise,
                ; delay realizing the value in m1 so we merge on demand instead.
                [false true]
                (let [m2v (get m2 k)]
                  (if (map? m2v)
                    (delay
                      (let [m1v (get m1 k)]
                        (if (map? m1v)
                          (inner-merge m1v m2v)
                          m2v)))
                    m2v))

                ; is not realized in m2 yet, we have to delay accessing any of it.
                ([true false] [false false])
                (delay
                  (let [m2v (get m2 k)]
                    (if (map? m2v)
                      (let [m1v (get m1 k)]
                        (if (map? m1v)
                          (inner-merge m1v m2v)
                          m2v))
                      m2v))))

              [true false]
              (if (realized-at? m1 k)
                (get m1 k)
                (delay (get m1 k)))

              [false true]
              (if (realized-at? m2 k)
                (get m2 k)
                (delay (get m2 k)))

              [false false] nil))

          (inner-merge [m1 m2]
            (->> (sets/union (keys m1) (keys m2))
                 (into {} (map (fn [k] [k (value m1 m2 k)])))
                 (->LazyMap)))]

    (reduce inner-merge (->LazyMap {}) (remove empty? maps))))
