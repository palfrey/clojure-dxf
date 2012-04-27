(ns dxf
  (:use [clojure.string :only [join upper-case]])
  )

(defn Style []
  {
   :kind :style
   :name "standard"
   :flag 0
   :height 0
   :widthFactor 40
   :obliqueAngle 50
   :mirror 0
   :lastHeight 1
   :font "arial.ttf"
   :bigFont ""
   })

(defn Layer []
  {
   :kind :layer
   :name "PYDXF"
   :color 7
   :lineType "continuous"
   :flag 64
   })

(defn LineType []
  {
   :kind :linetype
   :name "continuous"
   :description "Solid line"
   :elements []
   :flag 64
   })

(defn Drawing []
  {
   :kind :drawing
   :insbase [0.0,0.0,0.0]
   :extmin [0.0,0.0]
   :extmax [0.0,0.0]
   :layers [(Layer)]
   :linetypes [(LineType)]
   :styles [(Style)]
   :blocks []
   :views []
   :entities []
   })

(defn nextline [] "\r\n")

(defn section [name x]
  (let
    [xstr (
           if (or (nil? x) (= (count x) 0))
          ""
           (str (nextline) (join (nextline) x))
           )
     ]
    (str 
      "0" (nextline) "SECTION" (nextline)
      "2" (nextline) (upper-case name)
      xstr (nextline)
      "0" (nextline) "ENDSEC"
    )
  )
)

(defn table [name x]
  (let
    [
     xstr (if (or (nil? x) (= (count x) 0)) "" (str (nextline) (join (nextline) x)))
     xlen (if (nil? x) (0) (count x))
    ]
    (str
      "0" (nextline) "TABLE" (nextline)
      "2" (nextline) (upper-case name) (nextline)
      "70" (nextline) xlen xstr (nextline)
      "0" (nextline) "ENDTAB")
  )
)

(defn acadver []
  (str
    "9" (nextline) "$ACADVER" (nextline)
    "1" (nextline) "AC1006"
  )
)

(defn point
  ([x] (point x 0))
  ([x index]
  (join (nextline)
        (map #(str (first %) (nextline) (last %))
             (map 
               #(vector
                  (+ (* (+ % 1) 10) index)
                  (nth x %)
                )
              (range (count x))
            )
        )
  ))
)

(defmulti generate (fn [x] (:kind x)))

(defn drawing-name [x]
  (str "10" (nextline)  "$" (upper-case (subs (str x) 1))))

(defn drawing-point [name x]
  (str (drawing-name name) (nextline) (point (name x))))

(defmethod generate :drawing [d]
  (let [
        header (section "header" [(acadver) (drawing-point :insbase d) (drawing-point :extmin d) (drawing-point :extmax d)])
        tables (section "tables" [(table "ltype" (map generate (:linetypes d)))
                (table "layer" (map generate (:layers d)))
                (table "style" (map generate (:styles d)))
                (table "view" (map generate (:views d)))])

        blocks (section "blocks" (map generate (:blocks d)))
        entities (section "entities" (map generate (:entities d)))]
        (str (join (nextline) [header tables blocks entities "0" "EOF"]) (nextline) "\n")
  )
)

(defmethod generate :layer [l]
  (str
    "0" (nextline) "LAYER" (nextline)
    "2" (nextline) (upper-case (:name l)) (nextline)
    "70" (nextline) (:flag l) (nextline)
    "62" (nextline) (:color l) (nextline)
    "6" (nextline) (:lineType l)
  ))

(defmethod generate :style [s]
    (str
      "0" (nextline) "STYLE" (nextline)
      "2" (nextline) (upper-case (:name s)) (nextline)
      "70" (nextline) (:flag s) (nextline)
      "40" (nextline) (:flag s) (nextline)
      "41" (nextline) (:widthFactor s) (nextline)
      "50" (nextline) (:obliqueAngle s) (nextline)
      "71" (nextline) (:mirror s) (nextline)
      "42" (nextline) (:lastHeight s) (nextline)
      "3" (nextline) (upper-case (:font s)) (nextline)
      "4" (nextline) (upper-case (:bigFont s))
    ))

(defmethod generate :linetype [l]
  (str
    "0" (nextline) "LTYPE" (nextline)
    "2" (nextline) (upper-case (:name l)) (nextline)
    "70" (nextline) (:flag l) (nextline)
    "3" (nextline) (:description l) (nextline)
    "72" (nextline) "65" (nextline)
    "73" (nextline) (count (:elements l)) (nextline)
    "40" (nextline) "0.0"))

