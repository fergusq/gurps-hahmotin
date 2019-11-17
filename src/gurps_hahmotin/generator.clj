(ns gurps-hahmotin.generator
  (:require [clojure.string :as str]
            [gurps-hahmotin.core :as c]
            [gurps-hahmotin.templates :as t]))

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

(defn prompt-optional-CP [options & {:keys [prompter] :or {prompter prompt}}]
  (prompter options :formatter #(str/join ", " (for [[skill cp] %]
                                                 (str (c/render-name skill) " " cp)))))

(defn prompt-template [templates & {:keys [prompter] :or {prompter prompt}}]
  (let [choice (prompter (conj templates {:name "None"}) :formatter :name)]
    (as-> (get choice :used-CP {}) used-CP
      (reduce #(merge-with + %1 (prompt-optional-CP %2 :prompter prompter))
              used-CP
              (:optional-CP choice))
      (if (empty? (:lens choice))
        used-CP
        (merge-with + used-CP (prompt-template (:lens choice) :prompter prompter))))))
      

(defn prompt-character [& {:keys [prompter] :or {prompter prompt}}]
  (merge-with +
              (prompt-template (vals t/species) :prompter prompter)
              (prompt-template (vals t/backgrounds) :prompter prompter)
              (prompt-template (vals t/professions) :prompter prompter)))

(defn -main [outfile & flags]
  (let [flags-set (set flags)
        used-CP (prompt-character :prompter (if (contains? flags-set "--random")
                                              random-prompter
                                              prompt))
        character {:used-CP used-CP}
        html (c/render-character-sheet character)]
    (prn character)
    (spit outfile html)))
