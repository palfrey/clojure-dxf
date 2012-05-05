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

(deftest line
  (let [
        l (assoc (Line) :points [[0,0,0],[1,1,1]])
       ]
      (is (cmpDxf (addItem (Drawing) :entities l) "line.py"))))

(deftest text
  (let [
        t (assoc (assoc (Text) :text "Please donate!") :point [3,0,1])
       ]
      (is (cmpDxf (addItem (Drawing) :entities t) "text.py"))))

(deftest solid
  (let [
        s (assoc (assoc (Solid) :points [[4,4,0],[5,4,0],[7,8,0],[9,9,0]]) :color 3)
       ]
      (is (cmpDxf (addItem (Drawing) :entities s) "solid.py"))))

(deftest rectangle
  (let [
        r (merge (Rectangle) 
                 {
                  :point [2,2,2]
                  :width 4
                  :height 3
                  :color 6
                  :solid (assoc (Solid) :color 2)
                }
              )
       ]
      (is (cmpDxf (addItem (Drawing) :entities r) "rectangle.py"))))

(deftest style
  (is (cmpDxf (addItem (Drawing) :styles (Style)) "style.py")))

(deftest insert
  (let [
    i (merge (Insert)
             {
              :name "test"
              :point [3,3,3]
              :cols 5
              :colspacing 2
              }
        )
      ]
  (is (cmpDxf (addItem (Drawing) :entities i) "insert.py"))))

(deftest mtext
  (let [
    m (merge (Mtext)
             {
                :text (str "Click on Ads" (nextline) "multiple lines with mtext")
                :point [1,1,1]
                :color 5
                :rotation 90
              }
        )
      ]
  (is (cmpDxf (addItem (Drawing) :entities m) "mtext.py"))))
