(ns jepsen.mongodb.util-test
  "Utility functions unit tests."
  (:require [clojure.test :refer :all]
            [jepsen.mongodb.util :as util]))

(defn- sleep-or-error
  [counter millis]
  (try
    (if (= millis :throw)
      (throw (Exception. "intentionally thrown")))
      (do (Thread/sleep millis)
          millis)
    (finally (swap! counter inc))))

(deftest test-real-pmap
  (let [counter (atom 0)
        func (partial sleep-or-error counter)]
    (is (= [0 0] (doall (util/real-pmap func [0 0]))))
    (is (= @counter 2)))

  (let [counter (atom 0)
        func (partial sleep-or-error counter)]
    (is (thrown? Exception (doall (util/real-pmap func [:throw 1000]))))
    (is (= @counter 2)))

  (let [counter (atom 0)
        func (partial sleep-or-error counter)]
    (is (thrown? Exception (doall (util/real-pmap func [1000 :throw]))))
    (is (= @counter 2)))

  (let [counter (atom 0)
        func (partial sleep-or-error counter)]
    (is (thrown? Exception (doall (util/real-pmap func [:throw :throw]))))
    (is (= @counter 2))))
