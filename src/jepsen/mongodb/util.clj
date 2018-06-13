(ns jepsen.mongodb.util
  "Utility functions."
  (:require [jepsen.control :as c]))

(defmacro maybe-sudo
  "Evaluates forms with a particular user unless `test` is running without any
  virtualization."
  [test user & body]
  `(if (= :vm (:virt ~test))
     (c/sudo ~user ~@body)
     ~@body))

(defmacro maybe-su
  "sudo root ... unless `test` is running without any virtualization."
  [test & body]
  `(if (= :vm (:virt ~test))
     (c/su ~@body)
     ~@body))

(defn path-prefix
  "Returns the subdirectory of the working directory that should be used for the
  node's data directory"
  [test node & suffixes]
  (apply str (:working-dir test) "/" (name node) suffixes))

(defn retry-on-network-error
  "Calls `func` up to `1 + retries` times."
  [func retries]
  (loop [iter 0]
    (let [res (try
                {:value (func)}
                (catch com.mongodb.MongoSocketException e
                  (if (>= iter retries)
                    (throw e)
                    {:error e})))]
      (if (contains? res :value)
        (:value res)
        (recur (+ iter 1))))))

;; Adapted from the jepsen.util/real-pmap function of Jepsen version 0.1.8 with
;; modifications to ensure that all launched futures are waited on even if the
;; function throws an exception.
(defn real-pmap
  "Like pmap, but launches futures instead of using a bounded threadpool.

  If running `f` against any element of `coll` throws an exception, then this
  function throws the exception associated with the earliest element of `coll`."
  [f coll]
  (->> coll
       (map (fn launcher [x] (future (f x))))
       doall
       (map (fn [fut] (try {:value (deref fut)}
                           (catch Throwable t {:error t}))))
       doall
       (map (fn [res] (if (contains? res :error)
                        (throw (:error res))
                        (:value res))))))
