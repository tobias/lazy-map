[![Build Status](https://travis-ci.org/Guaranteed-Rate/lazy-map.svg?branch=master)](https://travis-ci.org/Guaranteed-Rate/lazy-map)
[![Clojars Project](https://img.shields.io/clojars/v/com.guaranteedrate/lazy-map.svg)](https://clojars.org/com.guaranteedrate/lazy-map)
[![codecov](https://codecov.io/gh/Guaranteed-Rate/lazy-map/branch/master/graph/badge.svg)](https://codecov.io/gh/Guaranteed-Rate/lazy-map)

> Maps that satisfy Clojure's map interactions but delay computing their values until the value is accessed.

## Summary

This library provides a new Clojure data type, the *lazy map*. Lazy
maps act just like regular (persistent) maps, except that their values
are not computed until they are requested.

## Usage

Start by requiring the namespace:

```clojure 

(require '[lazy-map.core :as lm])

```

You can then construct a lazy map using the `literal->lazy-map` macro.

```clojure 

(def m (lm/literal->lazy-map 
        {:a (do (println "resolved :a") "value :a")
         :b (do (println "resolved :b") "value :b")}))

; => {:a <unrealized>, :b <unrealized>}

```

When you request a value from the map, it will be evaluated and its
value will be cached:

```clojure

(:a m)

; => resolved :a
; => "value :a"

(:a m)
; => "value :a"

```

You can `assoc` values onto lazy maps just like regular maps. If you
`assoc` a delay, it will be treated as an unrealized value and not
forced until necessary:

```clojure

(assoc (lm/literal->lazy-map {}) :a 1 :b (delay 2))

; => {:a 1, :b <unrealized>}

```

Lazy maps are very lazy. In practice, this means they probably will
not compute their values until absolutely necessary. For example,
taking the `seq` of a lazy map does not force any computation, and map
entries have been made lazy as well:

```clojure

(def m (lm/literal->lazy-map 
        {:a (do (println "resolved :a") "value :a")
         :b (do (println "resolved :b") "value :b")}))

; => #'lazy-map.core/m
(dorun m)
; => nil
(keys m)
; => (:a :b)
(key (first m))
; => :a
(val (first m))
; => resolved :a
; => "value :a"

```

You can also initialize a lazy map from a regular map, where delays
are taken as unrealized values:

```clojure 

(lm/lazy-map {:a 1 :b (delay 2)})

; => {:a 1, :b <unrealized>}

```

There are also some utility functions for dealing with lazy maps. You
can use `force-map` to compute all values in a lazy map. Alternatively, 
you can use `freeze-map` to replace all the unrealized values with a 
placeholder. Here is an illustration:

```clojure 

(lm/force-map (lm/lazy-map {:a (delay :foo) :b :bar}))

; => {:a :foo, :b :bar}

(lm/force-map
  (lm/freeze-map :quux
     (lm/lazy-map {:a (delay :foo) :b :bar})))

; => {:a :quux, :b :bar}

```

Finally, lazy maps will automatically avoid realizing their values
when they are converted to strings using `str`, `pr-str`, and
`print-dup`. Please see the [unit tests](./test/lazy_map/core_test.clj) 
for more examples of the exact behavior of lazy maps.

## Comparisons

Features unique to [com.guaranteedrate/lazy-map](https://github.com/Guaranteed-Rate/lazy-map):

* More robust handling of laziness: all possible operations on maps
  are supported correctly (e.g. `seq` and `reduce-kv`)
* Pretty string representations that preserve laziness


Features unique to [malabarba/lazy-map](https://github.com/Malabarba/lazy-map-clojure):

* ClojureScript support
* Transform Java classes into lazy maps (methods become keys)
