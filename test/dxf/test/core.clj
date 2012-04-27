(ns dxf.test.core
  (:use [dxf])
  (:use [clojure.test])
  (:use [clojure.java.shell])
  )

(defn exec-python [path]
    (let [s (sh "python" 
             (str "/home/palfrey/src/clojure-dxf/test/dxf/test/" path)
             )]
      (if (= (:exit s) 0) (:out s) (throw (Exception. (str "error while running " path))))
      ))

(defn tmpfile [suff]
  (doto (java.io.File/createTempFile "dxf" suff) .deleteOnExit))

(defn write-to-temp [data]
  (let [t (tmpfile "diff")]
    (with-open [w (clojure.java.io/writer t)]
      (.write w data)
    )
    (.getPath t)))

(defn diffstrings [a b]
  (let [s (sh "diff" "-u" (write-to-temp a) (write-to-temp b))]
  (print (:out s))
  (= (:exit s) 0)))

(defn cmptext [a b]
  (if (= a b) true (diffstrings a b)))

(defn cmpDxf [newObj old]
    (cmptext (generate newObj) (exec-python old)))

(deftest drawing
    (is (cmpDxf (Drawing) "drawing.py")))
