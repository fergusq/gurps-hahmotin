(ns gurps-hahmotin.generator
  (:require [clojure.string :as str]
            [gurps-hahmotin.core :as c]
            [gurps-hahmotin.templates :as t]))

(defn merge-characters
  ([c] c)
  ([c1 c2] (-> (merge c1 c2)
               (assoc :used-CP (merge-with +
                                           (:used-CP c1)
                                           (:used-CP c2)))
               (assoc :notes (into (get c1 :notes []) (:notes c2)))))
  ([c c2 & cs] (reduce merge-characters c (cons c2 cs))))

(defn prompt [options & {:keys [message formatter]
                         :or {message "Select one of:"
                              formatter str}}]
  (let [options (vec options)]
    (println message)
    (dotimes [i (count options)]
      (println (str " " (inc i) ": " (formatter (get options i)))))
    (loop []
      (print ">")
      (.flush *out*)
      (let [c (Integer. (read-line))]
        (if (<= 1 c (count options))
          (get options (dec c))
          (recur))))))

(defn random-prompter [options & o]
  (rand-nth options))

(def ^:dynamic *prompter* prompt)

(defn prompt-optional-CP [options]
  (*prompter* options :formatter #(str/join ", " (for [[skill cp] %]
                                                   (str (c/render-name skill) " " cp)))))

(defn assoc-if-exists [to key from]
  (if (contains? from key)
    (assoc to key (get from key))
    to))

(defn prompt-template [templates]
  (let [template (*prompter* (conj templates {:name "None"})
                                 :formatter :name)
        choice (reduce (fn [c key] (assoc c key (*prompter* (get-in template [:optional key]))))
                       template
                       (keys (:optional template)))
        used-CP (reduce #(merge-with + %1 (prompt-optional-CP %2))
                        (get choice :used-CP {})
                        (:optional-CP choice))
        character {:used-CP used-CP}]
    (-> character
        (assoc-if-exists :species choice)
        (assoc-if-exists :background choice)
        (assoc-if-exists :profession choice)
        (assoc-if-exists :notes choice)
        (as-> c (if (empty? (:lens choice))
                  c
                  (merge-characters c (prompt-template (:lens choice))))))))
      

(defn prompt-character [& {:keys [prompter] :or {prompter prompt}}]
  (binding [*prompter* prompter]
    (merge-characters
     (prompt-template (vals t/species))
     (prompt-template (vals t/backgrounds))
     (prompt-template (vals t/professions)))))

(defn -main [outfile & flags]
  (let [flags-set (set flags)
        character (prompt-character :prompter (if (contains? flags-set "--random")
                                                random-prompter
                                                prompt))
        html (c/render-character-sheet character)]
    (prn character)
    (spit outfile html)))
