# ChlorineJs Core library

[![Master Build Status](https://api.travis-ci.org/chlorinejs/core-cl2.png?branch=master)](https://travis-ci.org/chlorinejs/core-cl2)
[![Develop Build Status](https://api.travis-ci.org/chlorinejs/core-cl2.png?branch=develop)](https://travis-ci.org/chlorinejs/core-cl2)

[Chlorine](http://github.com/chlorinejs/chlorine) (formerly Clojurejs) is a naive implementation of a Clojure subset language to Javascript translator.

# What does this project do?
Since v1.5.0, Chlorine repository now contains only the transpiler library which is written in Clojure. All Chlorine source files (.cl2 files) has been moved to this repository for faster development.

In summary, this repository is a port of Clojure's clojure.core library to the ChlorineJS language.

The [core-cl2](core-cl2) sub-directory is a normal Leiningen project directory which contains source code (in ChlorineJS) and test code (in both Clojure and ChlorineJS). The [test_runners](test_runners) directory, in turn, sets up an environment to compile and run javascript unit tests. With help of [testem](https://github.com/airportyh/testem), these tests are compiled and evaluated in browsers/phantomjs/node everytime a file is changed.

# Develop
Ensure you have  installed Java and NPM. Starts watcher by typing:
```bash
make watch
```
open an other terminal to run the test
```
make livetest
```

# Note
- Sometimes, you want to compile by a local version of [Chloric](https://github.com/chlorinejs/chloric) instead of the version deployed to NPM, then type this instead: `make local-watch`.
to start the watcher.
- Tests are run only against Firefox on travis from now on. Others (phantomjs, chromium, mocha) sometimes cause unknown errors on travis. Of course, developers may test on their machine all Testem supported environments with `make ci-test`.

# License

Copyright © 2013 Hoang Minh Thang.

core-cl2 library may be used under the terms of either the [GNU Lesser General Public License (LGPL)](http://www.gnu.org/copyleft/lesser.html) or the [Eclipse Public License (EPL)](http://www.eclipse.org/legal/epl-v10.html). As a recipient of core-cl2, you may choose which license to receive the code under.

# More information
This project is also an example of how to organize a Chlorine project with unit tests.

For more information see the chlorine [wiki](https://github.com/chlorinejs/chlorine/wiki).
