# langolier

Programming Language Detector.

## Usage

*In Development*

``` clojure
(:use [langolier.train])

(train "(defn idenity [x] x)" :clojure)
(train "public static void main" :java)

(classify "(defn main- [] (print 1))")
```

## License

Copyright © 2013

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
