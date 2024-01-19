(ns ica2.calculate
  (:gen-class)
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

;; Extracts the last name from a full name string, if the name is not empty.
(defn extract-last-name [full-name]
  (if (and full-name (not (empty? full-name)))
    (last (clojure.string/split full-name #" "))
    ""))

;; Groups passengers by their route and price by creating a key from these attributes.
(defn group-by-route-and-price [passengers]
  (group-by (fn [p] [(nth p 2) (nth p 3) (nth p 4)]) passengers))

;; Splits a group of passengers into families and non-family groups based on last names.
(defn split-into-families-and-groups [group]
  (let [grouped-by-last-name (group-by #(extract-last-name (first %)) group)
        ;; Separates groups into families (more than one member) and singles.
        {families true, singles false} (group-by #(> (count (second %)) 1) grouped-by-last-name)]
    (concat
     ;; Maps each family group to a structure with "family" group type.
     (map (fn [[_ members]] {:group-type "family" :members members}) families)
     ;; Maps non-family members to a single group structure if they exist.
     (when-let [single-members (seq (mapcat second singles))]
       [{:group-type "group" :members single-members}]))))

;; Prepares groups from data by first grouping by route and price, then splitting into families and groups.
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

;; Reads a CSV file and returns its content as a sequence of vectors.
(defn read-csv [file-path]
  (with-open [reader (io/reader file-path)]
    (doall (csv/read-csv reader))))

;; Groups trips by route and compiles statistics for each group.
;; The data is reduced into a map keyed by departure and destination with details of each group.
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

;; Counts the number of flights taken by each passenger, identified by name and year of birth.
(defn count-flights-per-passenger [data]
  (frequencies (map (fn [[name yob _ _ _]] [name yob]) data)))

;; Analyzes the route data for each group, calculating statistics for prices and flights.
(defn analyze-route [route-groups passenger-flights]
  (let [analyze-fn (fn [groups]
                     ;; Extract prices and flight frequencies for group members.
                     (let [prices (map :price groups)
                           flights (map #(reduce + (map (fn [[name yob]]
                                                          (get passenger-flights [name yob] 0))
                                                        (:members %))) groups)]
                       ;; Calculate statistics for prices and flights.
                       {:price-stats (analyze-group prices)
                        :flights-stats (analyze-group flights)}))]
    ;; Reduce the groups data into a map containing analysis for each group type.
    (reduce (fn [acc [group-type groups]]
              (assoc acc group-type (analyze-fn groups)))
            {}
            route-groups)))

;; Retrieves and analyzes data for a specific route from 'from' to 'to'.
(defn get-route-data [from to grouped-trips passenger-flights]
  (let [route-groups (get grouped-trips [from to])]
    ;; Perform analysis if route groups are available.
    (when route-groups
      (analyze-route route-groups passenger-flights))))

;; Calculates route data for a given 'from' and 'to' destination.
;; This includes data for both the direct and reverse routes.
(defn calculate-route-data [from to]
  (let [data (read-csv "src/ica2/sales_team_7.csv")  ;; Read CSV data.
        prepared-data (prepare-groups (rest data))  ;; Prepare groups from data.
        passenger-flights (count-flights-per-passenger (rest data))  ;; Count flights per passenger.
        grouped-trips (group-trips-by-route prepared-data)  ;; Group trips by route.
        direct-route-data (get-route-data from to grouped-trips passenger-flights)  ;; Get data for direct route.
        reverse-route-data (get-route-data to from grouped-trips passenger-flights)]  ;; Get data for reverse route.
    ;; Return either direct or reverse route data, whichever is available.
    (or direct-route-data
        reverse-route-data)))
