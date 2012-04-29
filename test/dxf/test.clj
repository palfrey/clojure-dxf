(ns dxf.test
  (:use [dxf])
  (:use [clojure.test])
  (:use [clojure.java.shell])
  )

(defn exec-python [path]
    (let [
            fp (str (System/getProperty "user.dir") "/test/dxf/scripts/" path)
            s (sh "python" fp)
         ]
      (if
        (= (:exit s) 0)
        (:out s)
        (throw (Exception. (str "error while running " fp)))
      )
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
    (= (:exit s) 0))
  )

(defn cmptext [a b]
  (if (= a b) true (diffstrings a b)))

(defn cmpDxf [newObj old]
  (cmptext (generate newObj) (exec-python old)))

(deftest drawing
  (is (cmpDxf (Drawing) "drawing.py")))

(deftest face
  (let [
        f (assoc (assoc (Face) :points [[0,0,0],[1,0,0],[1,1,0],[0,1,0]]) :color 4)
       ]
      (is (cmpDxf (addItem (Drawing) :entities f) "face.py"))))

(deftest circle
  (let [
        c (assoc (assoc (Circle) :center [1,1,0]) :color 3)
       ]
      (is (cmpDxf (addItem (Drawing) :entities c) "circle.py"))))
