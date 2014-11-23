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

(defn config
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
   
(defmulti var-tests
  "TestContainer.tests support for vars. To create custom test
   types, define vars that have :c.t.g/type metadata, and then add
   a matching var-tests method that returns a collection of tests."
  (fn [v] (:clojure.test.generative/type (meta v))))

(defmethod var-tests :defspec [^clojure.lang.Var v]
  [{:name  (-> (when-let [ns (.ns v)]
                 (str ns "/" (.sym v))
                 (.sym v))
               symbol)
    :f @v
    :inputs (fn []
              (repeatedly
               (fn []
                 (into [] (map #(%) (:clojure.test.generative/arg-fns (meta v)))))))}])

(defmethod var-tests nil [v] nil)

(defprotocol TestContainer
  (tests
   [_]
   "Returns a collection of generative tests, where a test is a map with
      :name     ns-qualified symbol
      :f       fn to test
      :inputs   fn returning a (possibly infinite!) sequence of inputs

   All input generation should use and gen/*rnd*
   if a source of pseudo-randomness is needed."))

(extend-protocol TestContainer
  clojure.lang.Var
  (tests [v] (var-tests v)))

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
  [{:keys [name f inputs]} msec seeds]
  (print (str "\n" name)) (flush)
  (let [start (Environment/TickCount)                               ;;; (System/currentTimeMillis)
        futs (mapv
              #(future
                (try
                 (binding [gen/*rnd* (System.Random. %)]            ;;; java.util.Random.
                   (loop [iter 0
                          [input & more] (inputs)]
                     (let [status {:iter iter :seed % :name name :input input}]
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
                            :else status))
                         (assoc status :exhausted true)))))))
              seeds)]
    (map deref futs)))

(defn run-n
  "Run tests in parallel on nthreads, dividing msec equally between the tests."
  [tests msec nthreads]
  (mapcat #(run-one % (/ msec (count tests)) (repeatedly nthreads next-seed)) tests))

(defn run-var
  [var msec nthreads]
  (run-n (var-tests var) msec nthreads))

(defn dir-tests
  "Returns all tests in dirs"
   [& dirs]
  (let [load (fn [s] (require s) s)]
    (->> (mapcat #(ns/find-namespaces-in-dir (System.IO.DirectoryInfo. ^String %)) dirs)   ;;; java.io.File.
         (map load)
         (apply find-vars-in-namespaces)
         (mapcat tests))))

(defn run-tests-in-dirs
  [{:keys [threads msec verbose]} & dirs]
  (reduce
   (fn [{:keys [failures iters tests]} result]
     (if (or verbose (:exception result))
       (pprint/pprint result)
       (print "."))
     (when (:exception result)
       (clr/print-stack-trace (:exception result)))                   ;;; .printStackTrace ^Throwable
     (flush)
     {:failures (+ failures (if (:exception result) 1 0))
      :iters (+ iters (:iter result))
      :tests (inc tests)})
   {:failures 0 :iters 0 :tests 0}
   (run-n (apply dir-tests dirs) msec threads)))

(defn -main
  "Command line entry point. Calls System.exit!"
  [& dirs]
  (if (seq dirs)
    (try
     (let [result (apply run-tests-in-dirs (config) dirs)]
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
     (let [result (apply run-tests-in-dirs (config) dirs)]
       (println "\n" result))
     (catch Exception t                                            ;;; Throwable
	   (prn (str "Exception: " (.Message t)))
       (clr/print-stack-trace t))                                  ;;; (.printStackTrace t)
     (finally
      (shutdown-agents)))
    (do
      (println "Specify at least one directory with tests")))) 
