(ns ica2.sales_routines
  (:require [ica2.broker :as broker])
  (:require [ica2.core :as your_engine]))

(def team_number 7)
(def search_ticket_function your_engine/prepare_travel_plan)
(broker/run team_number search_ticket_function)