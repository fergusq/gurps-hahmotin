(ns gurps-hahmotin.core
  (:require [clojure.xml :as xml]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.zip :as zip]
            [selmer.parser :as selmer]))

(def skills-xml (xml/parse "resources/Basic Set.skl"))
(def advantages-xml (xml/parse "resources/Basic Set.adq"))
(def extra-advantages-xml (xml/parse "resources/Extra.adq"))
(def spells-xml (xml/parse "resources/Magic.spl"))

(defn xml->map [tags updater xml-element]
  (->> (:content xml-element)
       (filter #(contains? tags (:tag %)))
       (reduce #(update %1 (:tag %2) (partial updater (:content %2))) {})))

(defn first-updater [x _] (first x))

(defn xml->default [default-xml]
  (let [default (xml->map #{:type :name :modifier} first-updater default-xml)]
    (if (= (:type default) "Skill")
      {:key (keyword (:name default))
       :modifier (Integer. (:modifier default))}
      {:key (keyword (:type default))
       :modifier (Integer. (:modifier default))})))

(def difficulties {"E" -1
                   "A" -2
                   "H" -3
                   "VH" -4})

(defn xml->skill [skill-xml]
  (let [skill (merge (xml->map #{:name :difficulty :reference} first-updater skill-xml)
                     (xml->map #{:default} #(cons (xml->default {:content %1}) %2) skill-xml))
        [base difficulty] (str/split (:difficulty skill) #"/")]
    (-> skill
        (assoc :base (keyword base))
        (assoc :modifier (get difficulties difficulty))
        (update :default merge))))

(defn xml->spell [spell-xml]
  (let [spell (xml->map #{:name :spell_class
                          :casting_cost :maintanance_cost
                          :casting_time :duration
                          :reference} first-updater spell-xml)]
    {:name (:name spell)
     :casting-cost (:casting_cost spell)
     :maintanance-cost (:maintanance_cost spell)
     :duration (:duration spell)
     :reference (:reference spell)
     :difficulty "H"
     :modifier (get difficulties "H") ; FIXME
     :base :SpellAttribute}))

(defn xml->advantage [advantage-xml]
  (let [adv (xml->map #{:name :base_points :points_per_level} first-updater advantage-xml)]
    (as-> {:name (:name adv)} v
      (if (:base_points adv)
        (assoc v :base-points (Integer. (:base_points adv)))
        v)
      (if (:points_per_level adv)
        (assoc v :points-per-level (Integer. (:points_per_level adv)))
        v))))

(defn skills->map [skills]
  (reduce #(assoc %1 (keyword (-> (:name %2)
                                  (str/replace #"[ ()\[\]{},;/#]" "")
                                  (str/replace #"@.+@" "")
                                  (str/replace #"@" "")))
                  %2)
          {}
          skills))

(def skills (->> (:content skills-xml)
                 (map xml->skill)
                 (skills->map)))

(def advantages (->> (:content advantages-xml)
                     (into (:content extra-advantages-xml))
                     (map xml->advantage)
                     (skills->map)))

(def spells (->> (:content spells-xml)
                 (map xml->spell)
                 (skills->map)))

;;; Character

(defn scale-points [points]
  (condp <= points
    20 7
    16 6
    12 5
    8 4
    4 3
    2 2
    1 1
    0))

(declare get-value)

(defn scale [character default attr cost]
  (let [def (if (keyword? default)
              (get-value character default)
              default)]
    (+ (long def) (quot (get character attr 0)
                         cost))))

(defmulti get-value (fn [_ id] id))

(defmethod get-value :ST [c _]
  (scale c 10 :ST 10))

(defmethod get-value :DX [c _]
  (scale c 10 :DX 20))

(defmethod get-value :IQ [c _]
  (scale c 10 :IQ 20))

(defmethod get-value :HT [c _]
  (scale c 10 :HT 10))

(defmethod get-value :Per [c _]
  (scale c :IQ :Per 5))

(defmethod get-value :Will [c _]
  (scale c :IQ :Will 5))

(defmethod get-value :MaxHP [c _]
  (scale c :ST :MaxHP 2))

(defmethod get-value :MaxFP [c _]
  (scale c :HT :MaxFP 2))

(defmethod get-value :MaxHP3 [c _]
  (quot (get-value c :MaxHP) 3))

(defmethod get-value :MaxFP3 [c _]
  (quot (get-value c :MaxFP) 3))

(defmethod get-value :Initiative [c _]
  (scale c :Per :Initiative 2))

(defmethod get-value :TmpSpeed [c _]
  (scale c (+ (get-value c :HT)
              (get-value c :DX))
         :Speed 5))

(defmethod get-value :Speed [c _]
  (/ (get-value c :TmpSpeed)
     4))

(defmethod get-value :BM [c _]
  (scale c :Speed :BM 5))

(defmethod get-value :Lift [c _]
  (let [liftingST (+ (get-value c :ST) (quot (get c :Lift 0)
                                             3))]
    (quot (* liftingST liftingST)
          5)))

(defmethod get-value :Thrust [c _]
  (let [ST (get-value c :ST)]
    (condp <= ST
      17 "1d+2"
      15 "1d+1"
      13 "1d"
      11 "1d-1"
      9 "1d-2"
      7 "1d-3"
      "1d-4")))

(defmethod get-value :Swing [c _]
  (let [ST (get-value c :ST)]
    (condp <= ST
      17 "5d-1"
      16 "2d+2"
      15 "2d+1"
      14 "2d"
      13 "2d-1"
      12 "1d+2"
      11 "1d+1"
      10 "1d"
      9 "1d-1"
      8 "1d-2"
      "1d-3")))

; FIXME: Does not work for characters who are both mages and clerics
(defmethod get-value :SpellAttribute [c _]
  (+ (get-value c :Magery)
     (get-value c :PowerInvestiture)))

(declare get-skill-value)
(declare get-advantage-value)

(defmethod get-value :default [character skill-name]
  (let [points (get character skill-name 0)
        skill-index (if (vector? skill-name)
                      (first skill-name)
                      skill-name)
        skill (get skills skill-index)
        adv (get advantages skill-index)]
    (cond
      (= points 0) 0
      skill (get-skill-value character skill points)
      adv (get-advantage-value character adv points)
      :else 0)))

(defn get-skill-value [character skill points]
  (+ (get-value character (:base skill))
     (:modifier skill)
     (scale-points points)))

(defn get-advantage-value [characters adv points]
  (let [base-points (or (:base-points adv) 0)
        points-per-level (or (:points-per-level adv) 9999)
        level (quot (- points base-points)
                    points-per-level)]
    (if (< (Math/abs points) (Math/abs base-points))
      nil
      level)))


(def sheet-file "resources/character-sheet/character_form_v3.2.html")

(def sheet (slurp sheet-file))

(def required-values #{:ST :DX :IQ :HT
                       :Per :Will
                       :MaxHP :MaxFP :MaxHP3 :MaxFP3
                       :Initiative :Speed
                       :BM :Lift
                       :Swing
                       :Thrust})

(defn render-simple-name [skill-name & [ref-name]]
  (let [rendered-name (cond
                        (not (nil? ref-name)) ref-name
                        (contains? skills skill-name) (:name (get skills skill-name))
                        (contains? advantages skill-name) (:name (get advantages skill-name))
                        :else (name skill-name))]
    (str/replace rendered-name #" *\(@.+@\)" "")))

(defn render-name [skill-name & [ref-name]]
  (if (vector? skill-name)
    (str/join " " (cons (name (render-simple-name (first skill-name) ref-name))
                        (map #(str "(" % ")") (rest skill-name))))
    (render-simple-name skill-name ref-name)))

(defn add-cp-to-context-map [character context-map kw]
  (let [used-CP (:used-CP character)
        value (get-value used-CP kw)
        skill-count (:skill-count context-map)
        advantage-count (:advantage-count context-map)
        lang-count (:lang-count context-map)
        skill-index (if (vector? kw)
                      (first kw)
                      kw)]
    (as-> context-map cm
      (assoc cm kw value)
      (assoc cm (keyword (str (render-name kw) "CP")) (get used-CP kw))
      (cond
        (= skill-index :Language)
        (-> cm
            (update :lang-count inc)
            (assoc-in [:languages lang-count] {:name (get kw 1)
                                               :CP (get used-CP kw)}))
        (contains? skills skill-index)
        (-> cm
            (update :skill-count inc)
            (assoc-in [:skills skill-count] (as-> (skill-index skills) v
                                              (assoc v :value value)
                                              (assoc v :CP (get used-CP kw))
                                              (assoc v :base (render-name (:base v)))
                                              (assoc v :name (render-name kw (:name v))))))
        
        (contains? advantages skill-index)
        (-> cm
            (update :advantage-count inc)
            (assoc-in [:advantages advantage-count] (as-> (skill-index advantages) v
                                                      (assoc v :CP (get used-CP kw))
                                                      (assoc v :name (render-name kw (:name v))))))
        (contains? required-values skill-index) cm
        :else (do
                (println "Unknown property:" skill-index)
                cm)))))
            

(defn create-context-map [character]
  (->> (clojure.set/union (->> character :used-CP keys set) required-values)
       (vec)
       (sort #(compare (render-name %1) (render-name %2)))
       (reduce (partial add-cp-to-context-map character)
               {:css "resources/character-sheet/character_form_v3.2.css"
                :species (:species character)
                :background (:background character)
                :profession (:profession character)
                :skill-count 0
                :advantage-count 0
                :lang-count 0
                :TotalCP (apply + (vals (:used-CP character)))
                :UnusedCP (apply - 150 (vals (:used-CP character)))})))

(defn render-character-sheet [character]
  (->> (create-context-map character)
       (selmer/render sheet)))
