(ns ica2.calculate
  (:gen-class)
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

;; Extract the last name from a full name safely.
(defn extract-last-name [full-name]
  (if (and full-name (not (empty? full-name)))
    (last (clojure.string/split full-name #" "))
    ""))

;; Group passengers by route and price.
(defn group-by-route-and-price [passengers]
  (group-by (fn [p] [(nth p 2) (nth p 3) (nth p 4)]) passengers))

;; Split groups into families and groups based on last names.
(defn split-into-families-and-groups [group]
  (let [grouped-by-last-name (group-by #(extract-last-name (first %)) group)
        {families true, singles false} (group-by #(> (count (second %)) 1) grouped-by-last-name)]
    (concat
     (map (fn [[_ members]] {:group-type "family" :members members}) families)
     (when-let [single-members (seq (mapcat second singles))]
       [{:group-type "group" :members single-members}]))))

;; Prepare groups from data.
(defn prepare-groups [data]
  (let [by-route-and-price (group-by-route-and-price (rest data))]
    (mapcat (fn [[route-and-price group]]
              (let [subgroups (split-into-families-and-groups group)]
                (map (fn [subgroup]
                       {:route route-and-price
                        :price (nth (first group) 4)
                        :group-type (:group-type subgroup)
                        :members (:members subgroup)})
                     subgroups)))
            by-route-and-price)))

;; Read CSV file.
(defn read-csv [file-path]
  (with-open [reader (io/reader file-path)]
    (doall (csv/read-csv reader))))

;; Group trips by route and analyze statistics
(defn group-trips-by-route [prepared-data]
  (reduce (fn [acc {:keys [route group-type price members]}]
            (let [[departure destination] route
                  price-num (Integer/parseInt price)]
              (update acc [departure destination]
                      #(update % group-type
                               (fnil conj [])
                               {:price price-num, :members members}))))
          {}
          prepared-data))

;; Analyze groups for statistics on prices and number of people.
(defn analyze-group [items]
  (when-not (empty? items)
    (let [items (map double items)] ;; Ensure all elements are numbers
      {:avg (/ (reduce + items) (count items))
       :min (apply min items)
       :max (apply max items)})))

(defn count-flights-per-passenger [data]
  (frequencies (map (fn [[name yob _ _ _]] [name yob]) data)))

(defn analyze-route [route-groups passenger-flights]
  (let [analyze-fn (fn [groups]
                     (let [prices (map :price groups)
                           counts (map #(count (:members %)) groups)
                           flights (map #(reduce + (map (fn [[name yob]]
                                                          (get passenger-flights [name yob] 0))
                                                        (:members %))) groups)]
                       {:price-stats (analyze-group prices)
                        :flights-stats (analyze-group flights)}))]
    (reduce (fn [acc [group-type groups]]
              (assoc acc group-type (analyze-fn groups)))
            {}
            route-groups)))

(defn calculate-route-data [from to group-type]
  (let [data (read-csv "src/ica2/sales_team_7.csv")
        prepared-data (prepare-groups (rest data))
        passenger-flights (count-flights-per-passenger (rest data))
        grouped-trips (group-trips-by-route prepared-data)
        route-groups (get grouped-trips [from to])
        analyzed-route (analyze-route route-groups passenger-flights)]
    (get-in analyzed-route [group-type])))
