# ChlorineJs Core library

[![Master Build Status](https://api.travis-ci.org/chlorinejs/core-cl2.png?branch=master)](https://travis-ci.org/chlorinejs/core-cl2)
[![Develop Build Status](https://api.travis-ci.org/chlorinejs/core-cl2.png?branch=develop)](https://travis-ci.org/chlorinejs/core-cl2)

[Chlorine](http://github.com/chlorinejs/chlorine) (formerly Clojurejs) is a naive implementation of a Clojure subset language to Javascript translator.

# What does this project do?
Since v1.5.0, Chlorine repository now contains only the transpiler library which is written in Clojure. All Chlorine source files (.cl2 files) has been moved to this repository for faster development.

In summary, this repository is a port of Clojure's clojure.core library to the ChlorineJS language.

# Develop
Ensure you have installed Leiningen and NPM. Starts watcher by typing:
```bash
lein cl2c auto dev
```
open an other terminal to run the test
```
lein npm run-script mocha-auto
```

# License

Copyright © 2013 Hoang Minh Thang.
Copyright © 2014 Hoang Minh Thang.

core-cl2 library may be used under the terms of either the [GNU Lesser General Public License (LGPL)](http://www.gnu.org/copyleft/lesser.html) or the [Eclipse Public License (EPL)](http://www.eclipse.org/legal/epl-v10.html). As a recipient of core-cl2, you may choose which license to receive the code under.

# More information

For more information see the chlorine [wiki](https://github.com/chlorinejs/chlorine/wiki).
