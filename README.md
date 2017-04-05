# Hokko

[![Build Status](https://travis-ci.org/tzbob/hokko.svg?branch=master)](https://travis-ci.org/tzbob/hokko)
[![codecov](https://codecov.io/gh/tzbob/hokko/branch/master/graph/badge.svg)](https://codecov.io/gh/tzbob/hokko)

Hokko is an experimental Scala Push/Pull FRP implementation[1].
It provides Non-Discrete Behaviors, Discrete Behaviors and Events.

## Repository

```core``` contains the core FRP library. It has the continuous, discrete and incremental behavior implementations as well as the event implementation in ```shared/src/main/scala/hokko/```. Syntactic sugar to make use of the type-class-based implementation can be found in ```shared/src/main/scala/hokko/syntax/```. An experimental helper front-end is available in ```shared/src/main/scala/hokko/control/```.

```collection``` contains the FRP incremental collection abstractions. ```shared/src/main/scala/hokko/collection/``` contains implementations for common collection operations.

```jvm/test``` contains test cases for both ```core``` and ```shared```. These can be run with sbt: ```sbt test``` and serve as examples of the library.

