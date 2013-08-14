(ns kiran.test-core
  (:use midje.sweet)
  (:require [kiran.core :as p]))

(defn under [num]
  (fn [p n] (or (< num n) (< num n))))


(def a (p/cell :a 0))
(def b (p/cell :b 0))
(def c (p/cell :c 0))
(def pg0 (p/connect [a b] c +))
(def pg1 (p/connect '+ [a b] c +))
(def pg2 (p/connect 'inc [a] b inc))
(def pg3 (p/connect 'inc [c] a inc (under 10)))
