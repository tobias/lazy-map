(ns lazy-map.core
  "Main namespace, contains utility functions, type definitions, and
  lazy map functions.

  Public API is `->LazyMap`, `->?LazyMap`, `lazy-map`, `force-map`, `freeze-map`, `merge`, `deep-merge`."
  (:require [clojure.pprint :as pp]
            [clojure.walk :as walk]
            [clojure.set :as sets])
  (:import java.io.Writer
           (java.util Map RandomAccess Map$Entry)
           (clojure.lang Seqable IPersistentMap
                         IPersistentCollection IMapIterable
                         ILookup IKVReduce IFn Associative
                         Sequential Reversible IPersistentVector
                         IPersistentStack Indexed IMapEntry IHashEq MapEntry IPersistentSet IPersistentList)
           (java.io Serializable))
  (:refer-clojure :exclude (merge)))

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
  (empty [this] false)
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

  Associative
  (containsKey [this k]
    (and contents
         (.containsKey contents k)))
  (entryAt [this k]
    (and contents
         (lazy-map-entry k (.valAt contents k))))

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
    (and contents
         (force (.valAt contents k))))
  (valAt [this k not-found]
    ;; This will not behave properly if not-found is a Delay,
    ;; but that's a pretty obscure edge case.
    (and contents
         (force (.valAt contents k not-found))))

  IMapIterable
  (keyIterator [this]
    (.iterator
      ^Iterable
      (or (keys contents) ())))
  (valIterator [this]
    (.iterator
      ;; Using the higher-arity form of map prevents chunking.
      ^Iterable
      (map (fn [[k v] _]
             (force v))
           contents
           (repeat nil))))

  IPersistentCollection
  (count [this]
    (if contents
      (.count contents)
      0))
  (empty [this]
    (or (not contents)
        (.empty contents)))
  (cons [this o]
    (LazyMap. (.cons (or contents {}) o)))
  (equiv [this o]
    (.equiv
      ^IPersistentCollection
      (into {} this) o))

  IPersistentMap
  (assoc [this key val]
    (LazyMap. (.assoc (or contents {}) key val)))
  (without [this key]
    (LazyMap. (.without (or contents {}) key)))

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

  Object
  (toString [this]
    (str (freeze-map (->PlaceholderText "<unrealized>") this))))

(alter-meta!
  #'->LazyMap assoc :doc
  "Turn a regular map into a lazy map. Any values that are delays
  are interpreted as values that have yet to be realized.")

(extend-print LazyMap #(.toString ^LazyMap %))

(defmethod pp/simple-dispatch lazy_map.core.LazyMap [obj]
  (pp/simple-dispatch (freeze-map (->PlaceholderText "<unrealized>") obj)))

(defmethod pp/simple-dispatch lazy_map.core.LazyMapEntry [obj]
  (pp/simple-dispatch
    (let [raw-value (.val_ obj)]
      (map-entry (key obj) (if (and (delay? raw-value)
                                    (not (realized? raw-value)))
                             (->PlaceholderText "<unrealized>")
                             (force raw-value))))))

(defmethod pp/simple-dispatch lazy_map.core.PlaceholderText [obj]
  (pr obj))

;;;; Functions for working with lazy maps

(defn ->?LazyMap
  "Behaves the same as ->LazyMap, except that if m is already a lazy
  map, returns it directly. This prevents the creation of a lazy map
  wrapping another lazy map, which (while not terribly wrong) is not
  the best idea."
  [map]
  (if (instance? LazyMap map)
    map
    (->LazyMap map)))

(defprotocol LazyRewrite
  (rewrite [form]
    "Rewrites form into a lazy construct."))

(extend-protocol LazyRewrite
  Object
  (rewrite [form]
    form)
  MapEntry
  (rewrite [[k v]]
    [(rewrite k) `(delay ~(rewrite v))])
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
    `(->LazyMap (hash-map ~@(mapcat rewrite form)))))


(defmacro lazy-map
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
      (if (map? form)
        (into {} form)
        form))
    map))

(defn freeze-map
  "Replace all the unrealized values in a lazy map with placeholders,
  returning a regular map. No matter what is done to the returned map,
  the values in the original map will not be forced. v can be an
  object to use for all the values or a function of the key."
  [val map]
  (let [val (if (fn? val)
              val
              (constantly val))]
    (reduce-kv (fn [m k v]
                 (assoc m k (if (and (delay? v)
                                     (not (realized? v)))
                              (val k)
                              (force v))))
               {}
               (.contents ^LazyMap map))))


(defn merge
  "Merges two lazy maps. Preserves laziness of value access."
  [m1 m2]
  (letfn [(value [k]
            (case [(contains? m1 k) (contains? m2 k)]
              [true true] (if-some [v (get m2 k)] v (get m1 k))
              [true false] (get m1 k)
              [false true] (get m2 k)
              [false false] nil))]
    (->> (sets/union (keys m1) (keys m2))
         (into {} (map (fn [k] [k (delay (value k))])))
         (->LazyMap))))


(defn deep-merge
  "Deep merges two lazy maps. Preserves laziness of value access."
  [m1 m2]
  (letfn [(value [k]
            (case [(contains? m1 k) (contains? m2 k)]
              [true true] (if-some [m2v (get m2 k)]
                            (if (map? m2v)
                              (let [m1v (get m1 k)]
                                (if (map? m1v)
                                  (deep-merge m1v m2v)
                                  m2v))
                              m2v)
                            (get m1 k))
              [true false] (get m1 k)
              [false true] (get m2 k)
              [false false] nil))]
    (->> (sets/union (keys m1) (keys m2))
         (into {} (map (fn [k] [k (delay (value k))])))
         (->LazyMap))))
