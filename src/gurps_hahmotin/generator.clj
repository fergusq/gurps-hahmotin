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

(defn prompt-optional-CP [options]
  (prompt options :formatter #(str/join ", " (for [[skill cp] %]
                                               (str (c/render-name skill) " " cp)))))

(defn prompt-template [templates]
  (let [choice (prompt (conj templates {:name "None"}) :formatter :name)]
    (as-> (get choice :used-CP {}) used-CP
      (reduce #(merge-with + %1 (prompt-optional-CP %2)) used-CP (:optional-CP choice))
      (if (empty? (:lens choice))
        used-CP
        (merge-with + used-CP (prompt-template (:lens choice)))))))
      

(defn prompt-character []
  (merge-with +
              (prompt-template (vals t/species))
              (prompt-template (vals t/backgrounds))
              (prompt-template (vals t/professions))))

(defn -main [outfile]
  (let [used-CP (prompt-character)
        character {:used-CP used-CP}
        html (c/render-character-sheet character)]
    (prn character)
    (spit outfile html)))
