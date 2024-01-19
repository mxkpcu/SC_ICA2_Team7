(ns ica2.core
  (:require [ica2.calculate :as calc]))

(defrecord City [city price])
(defrecord Route [path total-cost connections])

(def city-data
  {"Krakov" [(City. "Warsaw" 100)]
   "Hamburg" [(City. "Berlin" 100)]
   "Warsaw" [(City. "Berlin" 300)
             (City. "Krakov" 100)
             (City. "Budapest" 400)]
   "Prague" [(City. "Berlin" 200)
             (City. "Brno" 100)
             (City. "Budapest" 300)
             (City. "Vienna" 200)]
   "Berlin" [(City. "Hamburg" 100)
             (City. "Warsaw" 300)
             (City. "Prague" 200)
             (City. "Munich" 100)
             (City. "Budapest" 300)]
   "Munich" [(City. "Berlin" 100)
             (City. "Innsbruck" 100)
             (City. "Zagreb" 400)]
   "Vienna" [(City. "Innsbruck" 200)
             (City. "Budapest" 300)
             (City. "Rome" 400)
             (City. "Prague" 200)
             (City. "Rijeka" 400)
             (City. "Zagreb" 300)]
   "Napoli" [(City. "Rome" 200)
             (City. "Rijeka" 100)]
   "Rijeka" [(City. "Zagreb" 100)
             (City. "Napoli" 100)
             (City. "Vienna" 400)]
   "Budapest" [(City. "Rome" 400)
               (City. "Berlin" 300)
               (City. "Vienna" 300)
               (City. "Warsaw" 400)
               (City. "Zagreb" 200)
               (City. "Prague" 300)]
   "Zagreb" [(City. "Budapest" 200)
             (City. "Rijeka" 100)
             (City. "Vienna" 300)
             (City. "Munich" 400)]
   "Innsbruck" [(City. "Rome" 400)
                (City. "Munich" 100)
                (City. "Vienna" 200)]
   "Rome" [(City. "Vienna" 400)
           (City. "Napoli" 200)
           (City. "Innsbruck" 400)
           (City. "Budapest" 400)]
   "Brno" [(City. "Prague" 100)]})

;; Calculates the number of connections in a path excluding the destination
(defn get-connections [path-without-destination]
  (- (+ (count path-without-destination) 1) 2))

;; Constructs a route object from a given path, destination, and total cost
(defn get-route [path to total-cost]
  (let [final-path (if (= (last path) to) path (conj path to))]
    (Route. final-path
            total-cost
            (get-connections final-path))))

;; Recursive function to find all routes from 'from' to 'to' in 'city-data'
(defn find-all-routes [city-data from to]
  (letfn [(dfs [current cost visited current-path]
            (if (= current to)
              (let [path (conj current-path to)]
                [(get-route path to cost)])
              (let [connections (get city-data current)]
                (apply concat
                       (for [next-city-data connections
                             :let [next-city-name (:city next-city-data)
                                   next-city-cost (:price next-city-data)]
                             :when (not (contains? visited next-city-name))]
                         (dfs next-city-name
                              (+ cost next-city-cost)
                              (conj visited next-city-name)
                              (conj current-path current)))))))]
    (dfs from 0 #{from} [])))

;; Retrieves analysis data for a route from 'from' to 'to'
(defn get-analysis-data [from to]
  (let [analysis-data (calc/calculate-route-data from to)]
    analysis-data))

;; Checks if all people in the list belong to the same family (have the same last name)
(defn is-family? [people]
  (= 1 (count (distinct (map #(calc/extract-last-name (first %)) people)))))

;; Prepares a travel plan based on departure and destination cities and people traveling
(defn prepare_travel_plan [departure_city destination_city people]
  (let [group-type (if (is-family? people) "family" "group")
        analysis-data (get-analysis-data departure_city destination_city)]
    (if (seq analysis-data)
      (let [group-analysis-data (get analysis-data group-type)
            max-price (* 0.7 (get-in group-analysis-data [:price-stats :max]))
            max-connections (get-in group-analysis-data [:flights-stats :max])
            routes (find-all-routes city-data departure_city destination_city)]
        ;; Print analysis data
        ;; (println "Analysis Data for" group-type "route" departure_city "->" destination_city ":price" max-price ":max-connections" max-connections)
        (let [filtered-routes (filter #(and (<= (:total-cost %) max-price)
                                            (<= (:connections %) max-connections))
                                      routes)]
          ;; Print filtered routes
          ;; (println "Filtered routes:" filtered-routes)
          (if (empty? filtered-routes)
            0
            (let [max-price-routes (filter #(= (:total-cost %) (apply max (map :total-cost filtered-routes))) filtered-routes)
                  chosen-route (first (sort-by :connections max-price-routes))]
              ;; Print chosen route
              ;; (println "Chosen route:" chosen-route)
              (if chosen-route
                (:total-cost chosen-route)
                0)))))))) ;; Return 0 if no route is found