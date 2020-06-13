(ns lazy-map.core-test
  (:require [lazy-map.core :refer :all]
            [clojure.test :refer :all]
            [clojure.pprint :as pp])
  (:refer-clojure :exclude (merge))
  (:import (java.util Map)))

(defmacro error
  [& [msg]]
  `(let [msg# ~msg]
     (throw (if msg#
              (AssertionError. msg#)
              (AssertionError.)))))

(deftest map-entry-test
  (is (= (key (map-entry :a :b)) :a))
  (is (= (val (map-entry :a :b)) :b))
  (is (= (vec (map-entry nil nil)) [nil nil])))

(defn make-lazy-map
  [& [constructor]]
  ((or constructor lazy-map)
   {:a (delay (error "value :a should not be realized"))
    :b 50
    :c (delay (error "value :c should not be realized"))}))

(deftest lazy-map-test
  (testing "lazy maps are lazy"
    (is (= (:b (make-lazy-map))
           50))
    (is (= (with-out-str
             (:c (lazy-map
                   {:a (delay (error "value :a should not be realized"))
                    :b 50
                    :c (delay (print "value :c was realized"))})))
           "value :c was realized")))
  (testing "lazy maps can be called as fns"
    (is (= (let [m (lazy-map
                     {:a 1
                      :b 2})]
             (m :b))
           2)))
  (testing "keys and vals work on lazy maps"
    (is (= (set
             (keys (make-lazy-map)))
           #{:a :b :c}))
    (is (= (->> (lazy-map
                  {:a (delay (println "value :a was realized"))
                   :b (delay (println "value :b was realized"))
                   :c (delay (println "value :c was realized"))})
                (vals)
                (take 2)
                (dorun)
                (with-out-str)
                (re-seq #"value :[a-c] was realized")
                (count))
           2)))
  (testing "assoc and dissoc work with lazy maps"
    (is (= (-> (make-lazy-map)
               (assoc :d (delay (error "value :d should not be realized")))
               (keys)
               (set))
           #{:a :b :c :d}))
    (is (= (-> (make-lazy-map)
               (dissoc :a :b)
               (keys)
               (set))
           #{:c})))
  (testing "lazy maps support default value for lookup"
    (is (= (:d (make-lazy-map) :default)
           :default))
    (is (= (get (make-lazy-map) :d :default)
           :default)))
  (testing "seqs for lazy maps do not contain delays"
    (is (= (set (lazy-map
                  {:a 1
                   :b (delay 2)}))
           #{[:a 1] [:b 2]})))
  (testing "equality for lazy maps"
    (is (= (lazy-map
             {:a 1
              :b (delay 2)})
           {:a 1
            :b 2})))
  (testing "reduce and kv-reduce for lazy maps"
    (is (= (reduce (fn [m [k v]]
                     (assoc m k v))
                   {}
                   (lazy-map
                     {:a 1
                      :b (delay 2)}))
           {:a 1 :b 2}))
    (is (= (reduce-kv (fn [m k v]
                        (assoc m k v))
                      {}
                      (lazy-map
                        {:a 1
                         :b (delay 2)}))
           {:a 1 :b 2})))
  (testing "str representation of lazy maps"
    (is (= (str (lazy-map {:a 1}))
           "{:a 1}"))
    (is (= (str (lazy-map {:a (delay 1)}))
           "{:a <unrealized>}")))
  (testing "pr-str representation of lazy maps"
    (is (= (pr-str (lazy-map {:a 1}))
           "{:a 1}"))
    (is (= (pr-str (lazy-map {:a (delay 1)}))
           "{:a <unrealized>}")))
  (testing "pprint representation of lazy maps"
    (is (= (with-out-str (pp/pprint (lazy-map {:a 1})))
           (format "{:a 1}%n")))
    (is (= (with-out-str (pp/pprint (lazy-map {:a (delay 1)})))
           (format "{:a <unrealized>}%n"))))
  (testing "str representation of lazy map entries"
    (is (= (str (lazy-map-entry :a 1))
           "[:a 1]"))
    (is (= (str (lazy-map-entry :a (delay 1)))
           "[:a <unrealized>]")))
  (testing "pr-str representation of lazy map entries"
    (is (= (pr-str (lazy-map-entry :a 1))
           "[:a 1]"))
    (is (= (pr-str (lazy-map-entry :a (delay 1)))
           "[:a <unrealized>]")))
  (testing "pprint representation of lazy map entries"
    (is (= (with-out-str (pp/pprint (lazy-map-entry :a 1)))
           (format "[:a 1]%n")))
    (is (= (with-out-str (pp/pprint (lazy-map-entry :a (delay 1))))
           (format "[:a <unrealized>]%n"))))
  (testing "lazy-map macro"
    (is (= (with-out-str
             (let [m (literal->lazy-map
                       {:a (println "value :a was realized")
                        :b (println "value :b was realized")
                        :c (println "value :c was realized")})]
               (doseq [k [:b :c]]
                 (k m))))
           (format "value :b was realized%nvalue :c was realized%n"))))
  (testing "forcing a lazy map"
    (is (= (->> (literal->lazy-map
                  {:a (println "value :a was realized")
                   :b (println "value :b was realized")})
                (force-map)
                (with-out-str)
                (re-seq #"value :[ab] was realized")
                (set))
           #{"value :a was realized" "value :b was realized"})))
  (testing "freezing a lazy map"
    (= (->> (make-lazy-map)
            (freeze-map :foo))
       {:a :foo
        :b 50
        :c :foo})
    (= (->> (make-lazy-map)
            (freeze-map name))
       {:a "a"
        :b 50
        :c "c"}))
  (testing "keyIterator and valIterator"
    (is (= false (.hasNext (.keyIterator (literal->lazy-map {})))))
    (is (= true (.hasNext (.keyIterator (literal->lazy-map {:a 1})))))
    (is (= :a (.next (.keyIterator (literal->lazy-map {:a 1})))))
    (is (= :a (.next (.keyIterator (literal->lazy-map {:a 1})))))
    (is (= false (.hasNext (.valIterator (literal->lazy-map {})))))
    (is (= true (.hasNext (.valIterator (literal->lazy-map {:a 1})))))
    (is (= 1 (.next (.valIterator (literal->lazy-map {:a 1}))))))
  (testing "empty maps"
    (is (empty? (keys (literal->lazy-map {}))))
    (is (empty? (vals (literal->lazy-map {}))))
    (is (= (count (literal->lazy-map {})) 0))
    (is (lazy-map? (empty (literal->lazy-map {})))))

  (testing "literals are not wrapped in delays."
    (let [m (literal->lazy-map {:a :b})]
      (is (realized-at? m :a)))
    (let [m (literal->lazy-map {:a (+ 1 2 3)})]
      (is (not (realized-at? m :a))))))


(deftest laziness-of-nested-maps
  (let [effects (atom [])
        !       (fn [x] (swap! effects conj x) x)
        reset!! (fn [] (reset! effects []))]
    (testing "nested maps as map values"
      (let [m (literal->lazy-map {:a {:b (! :c)}})]
        (is (empty? @effects))
        (get m :a)
        (is (empty? @effects))
        (get-in m [:a :b])
        (is (= [:c] @effects)))
      (reset!!))

    (testing "nested maps within vectors"
      (let [m (literal->lazy-map {:a [{:b (! :c)}]})]
        (is (empty? @effects))
        (get m :a)
        (is (empty? @effects))
        (get-in m [:a 0])
        (is (empty? @effects))
        (get-in m [:a 0 :b])
        (is (= [:c] @effects)))
      (reset!!))

    (testing "nested maps within sets"
      (let [m (literal->lazy-map {:a #{{:b (! :c)}}})]
        (is (empty? @effects))
        (-> m :a)
        (is (empty? @effects))
        (-> m :a first)
        (is (empty? @effects))
        (-> m :a first :b)
        (is (= [:c] @effects)))
      (reset!!))

    (testing "nested maps within function calls"
      (let [m (literal->lazy-map {:a (do {:b (! :c)})})]
        (is (empty? @effects))
        (-> m :a)
        (is (empty? @effects))
        (-> m :a :b)
        (is (= [:c] @effects)))
      (reset!!))))

(deftest contains-value-doesnt-realize-unnecessarily
  (let [effects (atom [])
        !       (fn [x] (swap! effects conj x) x)
        m       (literal->lazy-map {:a :b :c (! :d)})]
    (is (empty? @effects))
    (is (.containsValue ^Map m :b))
    (is (empty? @effects))
    (is (.containsValue ^Map m :d))
    (is (= [:d] @effects))))

(deftest type-compliance
  (testing "is a java map"
    (is (instance? java.util.Map (lazy-map {:a (delay :b)}))))

  (testing "map methods"
    (is (= :b (.get (lazy-map {:a (delay :b)}) :a)))
    (is (.containsKey ^Map (lazy-map {:a (delay :b)}) :a))
    (is (= 0 (.size (lazy-map {}))))
    (is (= 1 (.size (lazy-map {:a :b}))))
    (is (.isEmpty (lazy-map {})))
    (is (not (.isEmpty (lazy-map {:a :b}))))
    (is (= #{:a} (.keySet (lazy-map {:a :b}))))
    (is (= #{(map-entry :a :b)} (.entrySet (lazy-map {:a :b}))))
    (is (= #{(map-entry :a :b)} (.entrySet (lazy-map {:a (delay :b)})))))

  (testing "equality between entry types when values are realized"
    (is (= (first {:a :b}) (first (lazy-map {:a :b}))))
    (is (not= (first {:a :b}) (first (lazy-map {:a :c})))))

  (testing "equality between entry types - realizes unrealized values"
    (is (= (first {:a 10}) (lazy-map-entry :a (delay (+ 1 2 3 4)))))
    (is (not= (first {:a 10}) (lazy-map-entry :a (delay (+ 1 2 3 4 5))))))

  (testing "equality between map types when values are realized"
    (is (= {:a :b} (lazy-map {:a :b})))
    (is (not= {:a :b} (lazy-map {:a :c}))))

  (testing "equality between map types - realizes unrealized values"
    (is (= {:a 10} (lazy-map {:a (delay (+ 1 2 3 4))})))
    (is (not= {:a 10} (lazy-map {:a (delay (+ 1 2 3 4 5))})))))


(deftest merge-test
  (testing "simple merge"
    (let [effects (atom [])
          !       (fn [x] (swap! effects conj x) x)
          m1      (literal->lazy-map {:a (! :a1) :b (! :b1)})
          m2      (literal->lazy-map {:a (! :a2) :c (! :c2)})
          merged  (merge m1 m2)]
      (is (empty? @effects))
      (is (= :a2 (get merged :a)))
      (is (= [:a2] @effects))
      (is (= :b1 (get merged :b)))
      (is (= [:a2 :b1] @effects))
      (is (= :c2 (get merged :c)))
      (is (= [:a2 :b1 :c2] @effects))))

  (testing "preservation of already realized values"
    (let [effects (atom [])
          !       (fn [x] (swap! effects conj x) x)
          a       (literal->lazy-map {:a :a1 :b (! :b1)})
          b       (literal->lazy-map {:b :b2})
          merged  (merge a b)]
      (is (empty? @effects))
      (is (not (realized-at? a :b)))
      (is (realized-at? b :b))
      (is (realized-at? merged :a))
      (is (= :a1 (get merged :a)))
      (is (empty? @effects))
      (is (realized-at? merged :b))
      (is (= :b2 (get merged :b)))
      (is (empty? @effects)))))

(deftest deep-merge-test
  (testing "simple deep merge"
    (let [effects (atom [])
          !       (fn [x] (swap! effects conj x) x)
          m1      (literal->lazy-map {:a (! :a1) :b (! :b1)})
          m2      (literal->lazy-map {:a (! :a2) :c (! :c2)})
          merged  (deep-merge m1 m2)]
      (is (empty? @effects))
      (is (= :a2 (get merged :a)))
      (is (= [:a2] @effects))
      (is (= :b1 (get merged :b)))
      (is (= [:a2 :b1] @effects))
      (is (= :c2 (get merged :c)))
      (is (= [:a2 :b1 :c2] @effects))))

  (testing "nested deep merge"
    (let [effects (atom [])
          !       (fn [x] (swap! effects conj x) x)
          m1      (literal->lazy-map {:a (! :a1) :b {:c (! :c1) :d (! :d1)}})
          m2      (literal->lazy-map {:a (! :a2) :b {:c (! :c2) :e (! :e2)}})
          merged  (deep-merge m1 m2)]
      (is (empty? @effects))
      (is (= :a2 (get merged :a)))
      (is (= [:a2] @effects))
      (is (= :c2 (get-in merged [:b :c])))
      (is (= [:a2 :c2] @effects))
      (is (= :d1 (get-in merged [:b :d])))
      (is (= [:a2 :c2 :d1] @effects))))

  (testing "preservation of already realized values"
    (let [effects (atom [])
          !       (fn [x] (swap! effects conj x) x)
          a       (literal->lazy-map {:a :a1 :b (! :b1) :c :c1 :d (! {:g :g1 :e :e1})})
          b       (literal->lazy-map {:b :b2 :c :c2 :d {:e :e2}})
          merged  (deep-merge a b)]
      (is (empty? @effects))
      (is (not (realized-at? a :b)))
      (is (realized-at? b :b))
      (is (realized-at? merged :a))
      (is (= :a1 (get merged :a)))
      (is (empty? @effects))
      (is (realized-at? merged :b))
      (is (= :b2 (get merged :b)))
      (is (empty? @effects))
      (is (= :c2 (get merged :c)))
      (is (empty? @effects))
      (is (= :e2 (get-in merged [:d :e])))
      (is (= [{:g :g1 :e :e1}] @effects))
      (is (= :g1 (get-in merged [:d :g]))))))