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
   [clojure.tools.namespace.find :as ns]              ;;; was just clojure.tools.namespace
   [clojure.data.generators :as gen] [clojure.test.generative.clr :as clr]  ;;; added clr
   [clojure.test.generative :as tgen]))
   
(set! *warn-on-reflection* true)

(def ^:private config-mapping
     [["clojure.test.generative.threads"
       [:nthreads]
       read-string
       (max 1 (dec Environment/ProcessorCount))]      ;;; (.availableProcessors (Runtime/getRuntime))
      ["clojure.test.generative.msec"
       [:msec]
       read-string
       10000]])

(defn config
  "Returns runner configuration derived from system properties."
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
   
(defprotocol Testable
  (get-tests [_]))
  
(extend-protocol Testable
  clojure.lang.Var 
  (get-tests
   [v]
   (let [m (meta v)
         arg-fns (::tgen/arg-fns m)
         specs (::tgen/specs m)]
     (cond
      arg-fns
      [{:test (-> (if-let [ns (.ns v)]
                    (str ns "/" (.sym v))
                    (.sym v))
                  symbol)
        :input-gen (fn []
                     (repeatedly
                      (fn []
                        (into [] (map #(%) arg-fns)))))}]

      specs
      @v))))

   
(defn- find-vars-in-namespaces
  [& nses]
  (when nses
    (reduce (fn [v ns] (into v (vals (ns-interns ns)))) [] nses)))

(defn- find-vars-in-dirs
  [& dirs]
  (let [nses (mapcat #(ns/find-namespaces-in-dir (System.IO.DirectoryInfo. ^String %)) dirs)]        ;;; java.io.File.
    (doseq [ns nses] (require ns))
    (apply find-vars-in-namespaces nses)))

(defn- run-one
  "Run f (presumably for side effects) repeatedly on n threads,
   until msec has passed or somebody throws an exception.
   Returns as many status maps as seeds passed in."
  [{:keys [test input-gen]} {:keys [msec seeds]}]
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

(defn- failed?
  "Does test result indicate a failure?"
  [result]
  (contains? result :exception))

(defn- run-n
  "Run tests in parallel on nthreads, dividing msec equally between the tests.
   Returns a list of maps of :iter, :seed"
  [{:keys [nthreads msec]} tests]
  (mapcat #(run-one %
                    {:msec (/ msec (count tests))
                     :seeds (repeatedly nthreads next-seed)})
          tests))

(def ^:private serializer (agent nil))

(defn serialized
  "Returns a function that calls f for side effects, async,
   serialized by an agent"
  ([f] (serialized f serializer))
  ([f agt]
     (fn [& args]
       (send-off agt
                 (fn [_]
                   (try
                    (apply f args)
                    (catch Exception t                  ;;; Throwable
                      (clr/print-stack-trace t)))      ;;; .printStackTrace
                   nil))
       nil)))

(def prf
  "Print and flush."
  (serialized (fn [s]
                (binding [*out* *err*]
                  (print s)
                  (flush)))))

(def print-stack-trace
  (serialized (fn [^Exception t] (clr/print-stack-trace t))))         ;;; Throwable  .printStackTrace

(def sprn (serialized prn))

(defn dir-tests
  "Returns all tests in dirs"
   [dirs]
  (let [load (fn [s] (require s) s)]
    (->> (mapcat #(ns/find-namespaces-in-dir (System.IO.DirectoryInfo. ^String %)) dirs)   ;;; java.io.File.
         (map load)
         (apply find-vars-in-namespaces)
         (mapcat get-tests))))

(defn inputs
  "For interactive use.  Returns an infinite sequence of inputs for
   a test."
  [test]
  ((:input-gen test)))

(defn run
  "Designed for interactive use.  Prints results to *out* and throws
   on first failure encountered."
  [nthreads msec & test-containers]
  (doseq [result (run-n {:nthreads nthreads
                         :msec msec}
                        (mapcat get-tests test-containers))]
    (if (failed? result)
      (throw (ex-info "Generative test failed" result))
      (prn result))))

(defn run-suite
  "Designed for test suite use."
  [{:keys [nthreads msec progress]} tests]
   (let [progress (or progress #(prf "."))
        ret (reduce
             (fn [{:keys [failures iters nresults]} result]
               (when (:exception result)
                 (print-stack-trace (:exception result)))
               (if (:exception result)
                 (sprn result)
                 (progress))
               {:failures (+ failures (if (:exception result) 1 0))
                :iters (+ iters (:iter result))
                :nresults (+ nresults 1)})
             {:failures 0 :iters 0 :nresults 0}
             (run-n {:nthreads nthreads
                     :msec msec}
                    tests))]
    (-> ret
        (assoc :tests (/ (:nresults ret) nthreads))
        (dissoc :nresults)))) 
 
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
