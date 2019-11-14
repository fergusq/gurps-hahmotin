(ns gurps-hahmotin.core
  (:require [clojure.xml :as xml]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.zip :as zip]))

(def skills-xml (xml/parse "resources/home/iikka/Lataukset/gcs-4.11.1/Library/Skills/Basic Set.skl"))

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

(def skills (->> (:content skills-xml)
                 (map xml->skill)
                 (reduce #(assoc %1 (keyword (str/replace (:name %2) #"[ ()\[\]{},/#]" "")) %2) {})))

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

(defn round [f]
  (let [i (int (Math/floor f))]
    (if (< i 0)
      (inc i)
      i)))

(defn scale [character default attr cost]
  (let [def (if (keyword? default)
              (get-value character default)
              default)]
    (+ (round def) (round (/ (get character attr 0)
                             cost)))))

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
  (let [liftingST (+ (get-value c :ST) (round (/ (:Lift c)
                                                 3)))]
    (round (/ (* liftingST liftingST)
              5))))

(defmethod get-value :default [character skill-name]
  (let [points (get character skill-name 0)
        skill (skill-name skills)]
    (if (= points 0)
      0 ; FIXME
      (+ (get-value character (:base skill))
         (:modifier skill)
         (scale-points points)))))
