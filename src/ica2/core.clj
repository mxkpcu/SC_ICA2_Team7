(ns ica2.core
  (:require [ica2.calculate :as calc]))

(defrecord City [city price])
(defrecord Route [path total-cost connections average-price average-connections])

(def from "Innsbruck")
(def destination "Warsaw")
(def group-type "group")

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

(defn get-connections [path-without-destination]
  (- (+ (count path-without-destination) 1) 2))

(defn get-route [path to total-cost average-price average-connections]
  (let [final-path (if (= (last path) to) path (conj path to))]
    (Route. final-path
            total-cost
            (get-connections final-path)
            average-price
            average-connections)))

(defn prettify-path [path]
  (->> path
       (map #(if (string? %) % (str (:city %))))
       (interpose " => ")
       (apply str)))

(defn find-all-routes [city-data from to group-type analysis-data]
  (let [visited (atom #{from})
        routes (atom [])
        current-path (atom [])]
    (letfn [(dfs [current cost]
              (if (= current to)
                (let [avg-price-data (get-in analysis-data [from to group-type "price-stats"])
                      avg-connections-data (get-in analysis-data [from to group-type "flights-stats"])
                      path (conj @current-path to)]
                  (if (and (<= cost (if avg-price-data (get avg-price-data "max") Double/POSITIVE_INFINITY))
                           (<= (count path) (if avg-connections-data (get avg-connections-data "max") Integer/MAX_VALUE)))
                    (swap! routes conj (get-route path to cost avg-price-data avg-connections-data))))
                (let [connections (get city-data current)]
                  (doseq [next-city-data connections]
                    (let [next-city-name (:city next-city-data)
                          next-city-cost (:price next-city-data)]
                      (if (contains? @visited next-city-name) nil
                          (do
                            (swap! visited conj next-city-name)
                            (swap! current-path conj current)
                            (dfs next-city-name (+ cost next-city-cost))
                            (swap! visited disj next-city-name)
                            (swap! current-path pop))))))))]
      (dfs from 0)
      @routes)))

(defn get-analysis-data [from to group-type]
  (let [analysis-data (calc/calculate-route-data from to group-type)]
    (println "Analysis Data for" group-type "route" from "->" to ":" analysis-data)
    analysis-data))

(defn get-ticket-info [from to group-type city-data]
  (let [analysis-data (get-analysis-data from to group-type)
        max-price (get-in analysis-data [:price-stats :max])
        max-connections (get-in analysis-data [:flights-stats :max])]
    (let [routes (find-all-routes city-data from to group-type analysis-data)
          filtered-routes (filter #(and (<= (:total-cost %) max-price)
                                        (<= (:connections %) max-connections))
                                  routes)]
      (println "\nAll routes that meet the criteria:")
      (doseq [route filtered-routes]
        (println "Route:" (prettify-path (:path route))
                 "Total price:" (:total-cost route) "$"
                 "Connections:" (:connections route)))
      (println "\nRecommended route based on analysis data:")
      (if (empty? filtered-routes)
        (println "No routes found that match the criteria.")
        (let [max-price-routes (filter #(= (:total-cost %) (apply max (map :total-cost filtered-routes))) filtered-routes)
              chosen-route (first (sort-by :connections max-price-routes))]
          (println "Route:" (prettify-path (:path chosen-route))
                   "Total price:" (:total-cost chosen-route) "$"
                   "Connections:" (:connections chosen-route)))))))

(get-ticket-info from destination group-type city-data)