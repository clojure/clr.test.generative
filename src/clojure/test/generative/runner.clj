;   Copyright (c) Rich Hickey, Stuart Halloway, and contributors.
;   All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.
;;; Modified for ClojureCLR by David Miller.
(ns clojure.test.generative.runner
  (:require
   [clojure.pprint :as pprint] [clojure.test.generative.clr :as clr]  ;;; added clr
   [clojure.tools.namespace.find :as ns]              ;;; was just clojure.tools.namespace
   [clojure.data.generators :as gen]))
   
(set! *warn-on-reflection* true)

(def ^:private config-mapping
     [["clojure.test.generative.threads"
       [:threads]
       read-string
       (max 1 (dec Environment/ProcessorCount))]      ;;; (.availableProcessors (Runtime/getRuntime))
      ["clojure.test.generative.msec"
       [:msec]
       read-string
       10000]])

(defn- config
  []
  (reduce
   (fn [m [prop path coerce default]]
     (let [val (Environment/GetEnvironmentVariable prop)]              ;;; (System/getProperty prop)
       (if (seq val)
         (assoc-in m path (coerce val))
         (assoc-in m path default))))
   {}
   config-mapping))

(def ^:private ^System.Random rnd (System.Random. (Environment/TickCount)))   ;;; ^java.util.Random (java.util.Random. (System/currentTimeMillis))

(defn- next-seed
  []
  (locking rnd
    (.Next rnd)))                ;;; .nextInt
   
(defprotocol TestContainer
  (get-tests [_]))
  
(extend-protocol TestContainer
  clojure.lang.Var 
  (get-tests
   [v]
   (when-let [arg-fns (:clojure.test.generative/arg-fns (meta v))]
     [{:test (-> (if-let [ns (.ns v)]
                   (str ns "/" (.sym v))
                   (.sym v))
                 symbol)
       :input-gen (fn []
                    (repeatedly
                     (fn []
                       (into [] (map #(%) arg-fns)))))}]))

  clojure.lang.MapEquivalence
  (get-tests
   [m] m))

   
(defn find-vars-in-namespaces
  [& nses]
  (when nses
    (reduce (fn [v ns] (into v (vals (ns-interns ns)))) [] nses)))

(defn find-vars-in-dirs
  [& dirs]
  (let [nses (mapcat #(ns/find-namespaces-in-dir (System.IO.DirectoryInfo. ^String %)) dirs)]        ;;; java.io.File.
    (doseq [ns nses] (require ns))
    (apply find-vars-in-namespaces nses)))

(defn run-one
  "Run f (presumably for side effects) repeatedly on n threads,
   until msec has passed or somebody throws an exception.
   Returns as many status maps as seeds passed in."
  [{:keys [test input-gen]} msec seeds]
  (prn) (prn test)
  (let [f (eval test)
        start (Environment/TickCount)                               ;;; (System/currentTimeMillis)   
        futs (mapv
              #(future
                (try
                 (binding [gen/*rnd* (System.Random. %)]            ;;; java.util.Random.
                   (loop [iter 0
                          [input & more] (input-gen)]
                     (let [status {:iter iter :seed % :test test :input input}]
                       (if input
                         (let [failure (try
                                        (apply f input)
                                        nil
                                        (catch Exception t          ;;; Throwable
                                          (assoc status :exception t) ))
                               now (Environment/TickCount)]         ;;; (System/currentTimeMillis)
                           (cond
                            failure failure
                            (< now (+ start msec)) (recur (inc iter) more)
                            :else (select-keys status [:test :seed :iter])))
                         (assoc status :exhausted true)))))))
              seeds)]
    (map deref futs)))

(defn run-n
  "Run tests in parallel on nthreads, dividing msec equally between the tests."
  [nthreads msec tests]
  (mapcat #(run-one % (/ msec (count tests)) (repeatedly nthreads next-seed)) tests))

(defn failed?
  "Does test result indicate a failure?"
  [result]
  (contains? result :exception))

(defn run-vars
  "Designed for interactive use.  Prints results to *out* and throws
   on first failure encountered."
  [nthreads msec & test-containers]
  (doseq [result (run-n nthreads msec (mapcat get-tests test-containers))]
    (if (failed? result)
      (throw (ex-info "Generative test failed" result))
      (prn result))))

(defn dir-tests
  "Returns all tests in dirs"
   [dirs]
  (let [load (fn [s] (require s) s)]
    (->> (mapcat #(ns/find-namespaces-in-dir (System.IO.DirectoryInfo. ^String %)) dirs)   ;;; java.io.File.
         (map load)
         (apply find-vars-in-namespaces)
         (mapcat get-tests))))

(defn run-suite
  "Designed for test suite use."
  [{:keys [threads msec verbose]} tests]
  (reduce
   (fn [{:keys [failures iters tests]} result]
     (if (or verbose (:exception result))
       (do (prn) (prn result))
       (print "."))
     (when (:exception result)
       (clr/print-stack-trace (:exception result)))                   ;;; .printStackTrace ^Throwable
     (flush)
     {:failures (+ failures (if (:exception result) 1 0))
      :iters (+ iters (:iter result))
      :tests (inc tests)})
   {:failures 0 :iters 0 :tests 0}
   (run-n threads msec tests)))

(defn -main
  "Command line entry point. Calls System.exit!"
  [& dirs]
  (if (seq dirs)
    (try
     (let [result (run-suite (config) (dir-tests dirs))]
       (println "\n" result)
       (Environment/Exit (:failures result)))	                  ;;; System/exit
     (catch Exception t                                           ;;; Throwable
	   (prn (str "Exception: " (.Message t)))
       (clr/print-stack-trace t)                                  ;;; (.printStackTrace t)
       (Environment/Exit -1))                                     ;;; System/exit
     (finally
      (shutdown-agents)))
    (do
      (println "Specify at least one directory with tests")
      (Environment/Exit -1))))                                    ;;; System/exit
	  
;;; ADDED

(defn -main-no-exit
  "Command line entry point. Does not call System.exit."
  [& dirs]
   (if (seq dirs)
    (try
     (let [result (run-suite (config) (dir-tests dirs))]
       (println "\n" result))
     (catch Exception t                                            ;;; Throwable
	   (prn (str "Exception: " (.Message t)))
       (clr/print-stack-trace t))                                  ;;; (.printStackTrace t)
     (finally
      (shutdown-agents)))
    (do
      (println "Specify at least one directory with tests")))) 
