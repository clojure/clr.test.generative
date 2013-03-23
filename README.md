# cljclr.test.generative #

A port of [clojure/test.generative](https://github.com/clojure/test.generative) library to ClojureCLR.

From the parent's README:

> Test data generation and execution harness. Very early days. This API will change. You have been warned.

# Releases

Nuget reference:

    PM> Install-Package clojure.test.generative

Leiningen/Clojars reference:

   [org.clojure.clr/test.generative "0.1.4"]
   
## Notes on the ported code ##

I have not yet ported clojure.test.generative.logback due to its reliance on very JVM-specific logging libraries.

   
# Copyright and License #

Original Clojure(JVM) code: 


>Copyright (c) 2012 Rich Hickey. All rights reserved.  The use and distribution terms for this software are covered by the Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the file epl-v10.html at the root of this distribution. By using this software in any fashion, you are agreeing to be bound bythe terms of this license.  You must not remove this notice, or any other, from this software.

Modified version for ClojureCLR:

> Copyright © 2013 David Miller All rights reserved. The use and distribution terms for this software are covered by the [Eclipse Public License 1.0] which can be found in the file epl-v10.html at the root of this distribution. By using this software in any fashion, you are agreeing to be bound by the terms of this license. You must not remove this notice, or any other, from this software.

[Eclipse Public License 1.0]: http://opensource.org/licenses/eclipse-1.0.php