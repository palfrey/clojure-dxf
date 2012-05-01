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

(defn Entity []
  {
   :layer "PYDXF"
  }
)

(defn Face []
  (merge (Entity) {
   :kind :face
   :points []
   }))

(defn Circle []
  (merge (Entity) {
   :kind :circle
   :center [0,0,0]
   :radius 1
  }))

(defn Line []
  (merge (Entity) {
   :kind :line
   :points []
   }))

(defn Solid []
  (merge (Entity) {
   :kind :solid
   :points []
   }))

(defn Text []
  (merge (Entity) {
    :kind :text
    :text ""
    :point [0,0,0]
    :height 1
    }))

(defn Rectangle []
  (merge (Entity) {
    :kind :rectangle
    :point [0,0,0]
    :width 1
    :height 1
    :line 1
    }))

(defn Insert []
  (merge (Entity) {
    :kind :insert
    :point [0,0,0]
  }))

(defn addItem [e key item]
  (assoc e key (conj (key e) item)))

(defn nextline [] "\r\n")

(defn section [name x]
  (let
    [xstr (
            if
            (or (nil? x) (= (count x) 0))
            []
            [(join (nextline) x)]
           )
     ]
    (join (nextline) (concat [
      "0" "SECTION" 
      "2" (upper-case name)]
      xstr
      ["0" "ENDSEC"])
    )
  )
)

(defn table [name x]
  (let
    [
     xstr (if
            (or (nil? x) (= (count x) 0))
            []
            [(join (nextline) x)]
          )
     xlen (if (nil? x) (0) (count x))
    ]
    (join (nextline) (concat [
      "0" "TABLE"
      "2" (upper-case name) 
      "70" xlen]
      xstr
      ["0" "ENDTAB"])
    )
  )
)

(defn acadver []
  (join (nextline) [
    "9" "$ACADVER"
    "1" "AC1006"
  ])
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

(defn points [p]
  (join (nextline) (map #(point (nth p %) %) (range (count p)))))

(defmulti generate (fn [x] (:kind x)))

(defn drawing-name [x]
  (str "10" (nextline) "$" (upper-case (subs (str x) 1))))

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
  (join (nextline) [
    "0" "LAYER"
    "2" (upper-case (:name l))
    "70" (:flag l)
    "62" (:color l)
    "6" (:lineType l)
  ]))

(defmethod generate :style [s]
    (join (nextline) [
      "0" "STYLE" 
      "2" (upper-case (:name s))
      "70" (:flag s)
      "40" (:flag s)
      "41" (:widthFactor s)
      "50" (:obliqueAngle s)
      "71" (:mirror s) 
      "42" (:lastHeight s)
      "3" (upper-case (:font s))
      "4" (upper-case (:bigFont s))
    ]))

(defmethod generate :linetype [l]
  (join (nextline) [
    "0" "LTYPE"
    "2" (upper-case (:name l))
    "70" (:flag l)
    "3" (:description l)
    "72" "65"
    "73" (count (:elements l))
    "40" "0.0"
  ]))

(defn common [c]
  (let [parent (:parent c c)]
    (str "8" (nextline) (:layer parent)
        (if (contains? parent :color) (str (nextline) "62" (nextline) (:color parent)) "")
        (if (contains? parent :extrusion) (str (nextline) (point (:extrusion parent) 200)))
        (if (contains? parent :lineType) (str (nextline) "6" (nextline) (:lineType parent)))
        (if (contains? parent :lineWeight) (str (nextline) "370" (nextline) (:lineWeight parent)))
        (if (contains? parent :lineTypeScale) (str (nextline) "48" (nextline) (:lineTypeScale parent)))
        (if (contains? parent :thickness) (str (nextline) "39" (nextline) (:thickness parent)))
    )))

(defmethod generate :face [f]
  (join (nextline) [
    "0" "3DFACE"
    (common f)
    (points (:points f))
  ]))

(defmethod generate :circle [c]
  (join (nextline) [
    "0" "CIRCLE"
    (common c)
    (point (:center c))
    "40" (:radius c)
  ]))

(defmethod generate :line [l]
  (join (nextline) [
    "0" "LINE"
    (common l)
    (points (:points l))
  ]))

(defmethod generate :text [t]
  (str
    (join (nextline) [
      "0" "TEXT"
      (common t)
      (point (:point t))
      "40" (:height t)
      "1" (:text t)
      ])
    (if (contains? t :rotation) (str (nextline) "50" (:rotation t)) "")
    (if (contains? t :xscale) (str (nextline) "41" (:xscale t)) "")
    (if (contains? t :obliqueAngle) (str (nextline) "51" (:obliqueAngle t)) "")
    (if (contains? t :style) (str (nextline) "7" (nextline) (upper-case (:name (:style t)))) "")
    (if (contains? t :flag) (str (nextline) "71" (nextline) (:flag t)) "")
    (if (contains? t :justifyhor) (str (nextline) "72" (nextline) (:justifyhor t)) "")
    (if (contains? t :alignment) (str (nextline) (point (:alignment t) 1)) "")
    (if (contains? t :justifyver) (str (nextline) "73" (nextline) (:justifyver t)) "")
  ))

(defmethod generate :solid [s]
  (join (nextline) [
      "0" "SOLID"
      (common s)
      (point (nth (:points s) 0) 0)
      (point (nth (:points s) 1) 1)
      (point (nth (:points s) 3) 2)
      (point (nth (:points s) 2) 3)
  ]))

(defmethod generate :rectangle [r]
  (let [p (:point r)
       points [
               p
               [(+ (nth p 0) (:width r)) (nth p 1) (nth p 2)]
               [(+ (nth p 0) (:width r)) (+ (nth p 1) (:height r)) (nth p 2)]
               [(nth p 0) (+ (nth p 1) (:height r)) (nth p 2)]
              p]
        ]
    (subs (str
         (if (contains? r :solid)
           (str (nextline)
               (generate
                 (merge
                   (Solid)
                   {
                    :points (subvec points 0 (- (count points) 1))
                    :parent (:solid r)
                    }
                  )
                )
            )
            ""
        )

        (if (contains? r :line) 
            (join "" (map #(str (nextline) (generate (merge (Line) {:points [(nth points %) (nth points (+ % 1))] :parent r}))) (range 4)))
        )
      ) 1)
  ))

(defmethod generate :insert [i]
  (str
    (join (nextline) [
        "0" "INSERT"
        "2" (:name i)
        (common i)
        (point (:point i))
    ])
      (if (contains? i :xscale) (str (nextline) "41" (nextline) (:xscale i)) "")
      (if (contains? i :yscale) (str (nextline) "42" (nextline) (:yscale i)) "")
      (if (contains? i :zscale) (str (nextline) "43" (nextline) (:zscale i)) "")
      (if (contains? i :rotation) (str (nextline) "50" (nextline) (:rotation i)) "")
      (if (contains? i :cols) (str (nextline) "70" (nextline) (:cols i)) "")
      (if (contains? i :colspacing) (str (nextline) "44" (nextline) (:colspacing i)) "")
      (if (contains? i :rows) (str (nextline) "71" (nextline) (:rows i)) "")
      (if (contains? i :rowspacing) (str (nextline) "45" (nextline) (:rowspacing i)) "")
    )
  )

