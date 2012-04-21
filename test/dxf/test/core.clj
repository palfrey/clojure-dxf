(ns clojure-dxf.test.core
  (:use [clojure-dxf.core])
  (:use [clojure.test])
  (:use [clojure.java.shell])
  )

(defn exec-python [path]
  (get (sh "python" (+ "/home/palfrey/src/clojure-dxf/test/clojure_dxf/test/" path))) :out)

(deftest drawing
    (is (= (.toString (Drawing.)) (exec-python "basic-face.py"))))
