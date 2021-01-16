(defproject org.clojure.clr/test.generative "1.0.1.1"
  :description "Port of clojure.org/test.generative to ClojureCLR"
  :url "https://github.com/clojure/clr.test.generative"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure.clr/tools.namespace "1.1.0.1"]
                 [org.clojure.clr/data.generators "1.0.1.1"]]
    :deploy-repositories [["clojars" {:url "https://clojars.org/repo/"
                                    :sign-releases  false}]]
  ;:source-paths [".lein-git-deps/cljclr.tools.namespace/src/"]
  :min-lein-version "2.0.0"
  :plugins [[lein-clr "0.2.0"]]
  :clr {:cmd-templates  {:clj-exe   [#_"mono" [CLJCLR15_40 %1]]
                         :clj-dep   [#_"mono" ["target/clr/clj/Debug 4.0" %1]]
                         :clj-url   "https://github.com/downloads/clojure/clojure-clr/clojure-clr-1.4.0-Debug-4.0.zip"
                         :clj-zip   "clojure-clr-1.4.0-Debug-4.0.zip"
                         :curl      ["curl" "--insecure" "-f" "-L" "-o" %1 %2]
                         :nuget-ver [#_"mono" [*PATH "nuget.exe"] "install" %1 "-Version" %2]
                         :nuget-any [#_"mono" [*PATH "nuget.exe"] "install" %1]
                         :unzip     ["unzip" "-d" %1 %2]
                         :wget      ["wget" "--no-check-certificate" "--no-clobber" "-O" %1 %2]}
        ;; for automatic download/unzip of ClojureCLR,
        ;; 1. make sure you have curl or wget installed and on PATH,
        ;; 2. uncomment deps in :deps-cmds, and
        ;; 3. use :clj-dep instead of :clj-exe in :main-cmd and :compile-cmd
        :deps-cmds      [; [:wget  :clj-zip :clj-url] ; edit to use :curl instead of :wget
                         ; [:unzip "../clj" :clj-zip]
                         ]
        :main-cmd      [:clj-exe "Clojure.Main.exe"]
        :compile-cmd   [:clj-exe "Clojure.Compile.exe"]})