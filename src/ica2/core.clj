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

(defn get-connections [path-without-destination]
  (- (+ (count path-without-destination) 1) 2))

(defn get-route [path to total-cost]
  (Route. (conj path to)
          total-cost
          (get-connections path)))

(defn prettify-path [path]
  (let [result (atom "")]
    (doseq [city path]
      (if (string? city)
        (reset! result (str @result city))
        (reset! result (str @result (:city city) " => " (:price city) "$" " => "))))
    @result))

(defn find-all-routes [city-data from to]
  (let [visited (atom #{from})
        routes (atom [])]
    (defn dfs [current path cost]
      (if (= current to)
        (swap! routes conj (get-route path to cost))

        (let [connections (get city-data current)]
          (doseq [next-city-data connections]
            (let [next-city-name (:city next-city-data)
                  next-city-cost (:price next-city-data)]
              (if (contains? @visited next-city-name) nil
                  (do
                    (reset! visited (conj @visited next-city-name))
                    (dfs next-city-name (conj path (City. current next-city-cost)) (+ cost next-city-cost))
                    (reset! visited (disj @visited next-city-name)))))))))
    (dfs from [] 0)
    @routes))

(defn does-satisfy [route max-price max-connection]
  (and (<= (:total-cost route) max-price) (<= (:connections route) max-connection)))

(defn get-ticket-info [from to group-type city-data]
  (let [routes (find-all-routes city-data from to)
        max-family-price 700
        max-family-connections 2
        max-tour-ptice 1000
        max-tour-connection 3
        sorted-routes (sort-by :total-cost routes)
        sorted-routes-desc (reverse sorted-routes)
        chosen-route (atom (Route. nil 0 0))]
    (if (= group-type "f")
      (do
        (println "\nFamily option\n— — — — — — — — — —")
        (println "Max price: " (str max-family-price "$"))
        (println "Max connections: " max-family-connections "\n— — — — — — — — — —\n")

        (doseq [route sorted-routes]
          (if (and
               (does-satisfy route max-family-price max-family-connections)
               (> (:total-cost route) (:total-cost @chosen-route)))
            (reset! chosen-route route)
            nil)

          (if (and
               (does-satisfy route max-family-price max-family-connections)
               (>= (:connections route) (:connections @chosen-route)))
            (reset! chosen-route route)
            nil))) nil)

    (if (= group-type "g")
      (do
        (println "\nGroup option \n— — — — — — — — — —")
        (println "Max price: " (str max-tour-ptice "$"))
        (println "Max connections: " max-tour-connection "\n— — — — — — — — — —\n")

        (doseq [route sorted-routes]
          (if (and
               (does-satisfy route max-tour-ptice max-tour-connection)
               (>= (:connections route) (:connections @chosen-route)))
            (reset! chosen-route route)
            nil)

          (if (and
               (does-satisfy route max-tour-ptice max-tour-connection)
               (> (:total-cost route) (:total-cost @chosen-route)))
            (reset! chosen-route route)
            nil))) nil)

    (if (not (or (= group-type "f") (= group-type "g"))) (println "Wrong group type. Please, try again.\n") nil)

    (if (> (count routes) 0)
      (do
        (println "Sorted routes from expensive to cheap: \n— — — — — — — — — —")
        (doseq [route sorted-routes-desc]
          (println "Path: " (prettify-path (:path route)) "\nTotal price: " (str (:total-cost route) "$") "\n— — — — — — — — — —")))
      (println "No routes found"))

    (if (> (count routes) 0)
      (if (= (:total-cost @chosen-route) 0)
        (println "Unfortunately, there is no route that meets your requirements.\n\n")
        (do
          (println (str "\nThe most expensive, extensive and suitable for all conditions ticket found:"))
          (println
           (prettify-path (:path @chosen-route))
           "\nTotal price:" (str (:total-cost @chosen-route) "$")
           "\nFlights:"
           (+ (:connections @chosen-route) 1)
           "\n\n")))
      nil)))

(println "Please, enter your departure city: ")
(def from (read-line))

(println "Please, enter your destination city: ")
(def destination (read-line))

(println "Please, enter your group type (f - family, g - group): ")
(def group-type (read-line))

(get-ticket-info from destination group-type city-data)
