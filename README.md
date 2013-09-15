# kiran

This is another implementation of the Sussman computation model. It is based off the version by Michael Nygard and Jim White.

Eric Schulte - http://repo.or.cz/w/propagator.git

Michael Nygard - https://github.com/mtnygard/clojure-snippets/tree/master/src/clj/propagator

Jim White - https://github.com/jimwhite/clojure-propagator


## Installation

Add `[im.chit/kiran "0.1.0"]` to your `project.clj` requirements 

## Usage

    (use kiran.core)

		(def a (p/cell :a 0))
		(def b (p/cell :b 0))
		(def c (p/cell :c 0))
		(def pg0 (p/connect [a b] c +))
		
		(a 1)
		@c  ;;=>  1

## License

Copyright © 2013 Chris Zheng

Distributed under the MIT License
