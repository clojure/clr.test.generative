(require
 '[clojure.data.generators :as gen]
 '[clojure.test.generative :as test :refer (defspec)]
 '[clojure.test.generative.runner :as runner])

;; generators have names that shadow core names of things generated
(gen/long)

;; generation is repeatable
(repeatedly
 2 
 #(binding [gen/*rnd* (System.Random. 42)]   ;;; java.util.Random.
   (gen/short)))

;; generation is composable
(gen/vec gen/short)

;; size is parameterized
(gen/vec gen/short 2)

;; size parameter can of course also be a generator
(gen/vec gen/short (gen/uniform 3 5))

;; generators are in scope as "types" in a defspec
(defspec longs-are-closed-under-increment
  inc ;; function under test
  [^long l]  ;; indicates generation via gen/long
  (assert (instance? System.Int64 %)))                 ;;; Long

;; specs are functions
(longs-are-closed-under-increment 4)

;; the next two steps are executed for you by the standard runner...

;; tests are extracted from vars
(def tests  (runner/get-tests #'longs-are-closed-under-increment))

(first tests)

;; run test with some generated inputs   ----- no longer public and args have changed
#_(runner/run-one
 (first tests)
 1000
 [42])

#_(runner/run-n
 2
 1000
 tests)

;; repl-friendly use
#_(runner/run-vars                               ;;; no longer exists
 2 1000 #'longs-are-closed-under-increment)

;; peek at what defspec tells us
(meta #'longs-are-closed-under-increment)

;; test that will fail
(defspec collections-are-small
  count
  [^{:tag (gen/vec gen/short (gen/uniform 0 25))} l]
  (assert (< % 20)))

;; run as from REPL
#_(runner/run-vars                                     ;;; args have changed
 2 1000 #'collections-are-small)
(ex-data *e)

;; run as from suite
(runner/run-suite {:nthreads 1 :msec 500} (runner/get-tests #'collections-are-small))   ;;; :threads



