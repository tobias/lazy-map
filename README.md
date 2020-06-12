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

You can then construct a lazy map using the `lazy-map` macro.

```clojure 

(def m (lm/lazy-map 
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

(assoc (lm/lazy-map {}) :a 1 :b (delay 2))

; => {:a 1, :b <unrealized>}

```

Lazy maps are very lazy. In practice, this means they probably will
not compute their values until absolutely necessary. For example,
taking the `seq` of a lazy map does not force any computation, and map
entries have been made lazy as well:

```clojure

(def m (lm/lazy-map 
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

(lm/->LazyMap {:a 1 :b (delay 2)})

; => {:a 1, :b <unrealized>}

```

You might prefer to use `->?LazyMap` instead of `->LazyMap`. The only
difference is that `->?LazyMap` acts as the identity function if you
pass it a map that is already lazy. This prevents nested lazy maps,
which are not inherently wrong but which could be bad for performance
if you nest them thousands of layers deep.

There are also some utility functions for dealing with lazy maps. You
can use `force-map` to compute all values in a lazy map. Alternatively, 
you can use `freeze-map` to replace all the unrealized values with a 
placeholder. Here is an illustration:

```clojure 

(lm/force-map (lm/->LazyMap {:a (delay :foo) :b :bar}))

; => {:a :foo, :b :bar}

(lm/force-map
  (lm/freeze-map :quux
     (lm/->LazyMap {:a (delay :foo) :b :bar})))

; => {:a :quux, :b :bar}

```

Finally, lazy maps will automatically avoid computing their values
when they are converted to strings using `str`, `pr-str`, and
`print-dup`.

Check out the [unit tests] for more information on the exact behavior of lazy maps.

[unit tests](./test/lazy_map/core_test.clj)

## Organization

All the code is currently in the `lazy-map.core` namespace, and the
unit tests are in the `lazy-map.core-test` namespace.

## See also

**[Malabarba's implementation] of lazy maps in Clojure.**

[Malabarba's implementation]: https://github.com/Malabarba/lazy-map-clojure

Features unique to `malabarba/lazy-map`:

* ClojureScript support
* Transform Java classes into lazy maps (methods become keys)

Features unique to `thinktopic/lazy-map`:

* More robust handling of laziness: all possible operations on maps
  are supported correctly (e.g. `seq` and `reduce-kv`)
* Pretty string representation and support for pretty-printing
