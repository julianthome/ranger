# ranger

ranger helps you to perform various operations (union, intersection, addition,
subtraction, ...) on boolean and numeric (scattered) ranges. Moreover, with
ranger you can translate numeric ranges into regular expression which can
be used to check whether a string represents a number that falls into a given
range. 

# Status

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)][licence]
[![Language](http://img.shields.io/badge/language-java-brightgreen.svg)][language]
[![Maven](https://maven-badges.herokuapp.com/maven-central/com.github.julianthome/ranger/badge.svg)][maven]
[![Linux Build Status](https://img.shields.io/travis/julianthome/ranger/master.svg?label=Linux%20build)][travis]
[![Windows Build status](https://img.shields.io/appveyor/ci/julianthome/ranger/master.svg?label=Windows%20build)][appveyor]
[![Test Coverage](https://codecov.io/gh/julianthome/ranger/branch/master/graph/badge.svg)][coverage]

[licence]: https://opensource.org/licenses/mit
[language]: https://www.java.com
[maven]: https://maven-badges.herokuapp.com/maven-central/com.github.julianthome/ranger
[travis]: https://travis-ci.org/julianthome/ranger
[appveyor]: https://ci.appveyor.com/project/julianthome/ranger
[coverage]: https://codecov.io/gh/julianthome/ranger 

# Usage

The following example illustrates how ranger can be used. The class
`AtomicNumRange` represents consecutive ranges whereas `NumRange` can represent
scattered, non-consecutive ranges (i.e., a set of `AtomicNumRange` objects).
For example, the range yielded by `r101.union(r4).toString()` would be
`[[+4,+4],[+100,+101]]{+4,+101}`. Note, that redundant `AtomicNumRanges` are
collapsed automatically. For example, `new AtomicNumRange(3,10).union(new
AtomicNumRange(2,13)).toString()` would yield the range `[+2,+13]`.


``` java
AtomicNumRange rinf = new AtomicNumRange();
System.out.println("1: " + rinf.toString());
AtomicNumRange r4 = new AtomicNumRange(4);
System.out.println("2: " + r4.toString());
AtomicNumRange r101 = new AtomicNumRange(100,101);
System.out.println("3: " + r101.union(r4).toString());
System.out.println("4: " + rinf.intersect(r4).toString());
System.out.println("5: " + new AtomicNumRange(3,10).union(new
        AtomicNumRange(2,13)).toString());
System.out.println("6: " + r101.union(r4).toRegex());
```

The output of the program above is depicted below.

``` bash
1: [-∞+0,+∞+0]
2: [+4,+4]
3: [[+4,+4],[+100,+101]]{+4,+101}
4: [+4,+4]
5: [+2,+13]
6: ([4-9]|[1-9][0-9]{1,})|([1-9][0-9]{2,})
```

In order to see more usage examples, please have a look at the [JUnit test
cases](https://github.com/julianthome/ranger/tree/master/src/test/java).


# Licence

The MIT License (MIT)

Copyright (c) 2017 Julian Thome <julian.thome.de@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
of the Software, and to permit persons to whom the Software is furnished to do
so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
