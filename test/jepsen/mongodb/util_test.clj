(ns jepsen.mongodb.util-test
  "Utility functions unit tests."
  (:require [clojure.test :refer :all]
            [jepsen.mongodb.util :as util]))

(defn- sleep-or-error
  [counter millisOrError]
  (try
    (cond
      (map? millisOrError) (throw (Exception. (str "intentionally thrown: "
                                                   (:throw millisOrError))))
      (= millisOrError :throw) (throw (Exception. "intentionally thrown"))
      :else (do (Thread/sleep millisOrError)
                millisOrError))
    (finally (swap! counter inc))))

(deftest test-real-pmap
  (testing "when no function invocation triggers an exception"
    (let [counter (atom 0)
          func (partial sleep-or-error counter)]
      (is (= [0 0] (doall (util/real-pmap func [0 0]))))
      (is (= @counter 2))))

  (testing "when the first function invocation triggers an exception"
    ; This test case is one that would fail with the version of the
    ; jepsen.util/real-pmap function in Jepsen 0.1.8 because it didn't wait for
    ; all launched futures before rethrowing the exception. The 1000ms sleep in
    ; the second future had caused it to still be running by the time we rethrow
    ; the exception from the first one.
    (let [counter (atom 0)
          func (partial sleep-or-error counter)]
      (is (thrown? Exception (doall (util/real-pmap func [:throw 1000]))))
      (is (= @counter 2))))

  (testing "when the second function invocation triggers an exception"
    (let [counter (atom 0)
          func (partial sleep-or-error counter)]
      (is (thrown? Exception (doall (util/real-pmap func [1000 :throw]))))
      (is (= @counter 2))))

  (testing "when both function invocations trigger an exception"
    (let [counter (atom 0)
          func (partial sleep-or-error counter)]
      (is (thrown-with-msg? Exception
                            #"first"
                            (doall (util/real-pmap func [{:throw "first"}
                                                         {:throw "second"}]))))
      (is (= @counter 2)))))
