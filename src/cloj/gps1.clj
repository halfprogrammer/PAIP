(ns cloj.gps1
  (:require [clojure.set :as s]))

(defrecord OP [action preconds add-list del-list])

(declare achieve-all)

(defn member? [a b]
  (a b))

(defn appropriate? [goal op]
  (member? (:add-list op) goal))

(defn select-keyval [func coll]
  (mapcat (fn [x]
            (when-let [y (func x)]
              (list (list x y))))
          coll))

(defn apply-op [state op ops]
  (when (achieve-all (:preconds op) state ops)
    (println (list 'executing (:action op)))
    (s/difference (s/union state (:add-list op))
                  (:del-list op))))

(defn achieve [goal state ops]
  (if (member? state goal)
    (list (list true state))
    (select-keyval #(apply-op state % ops)
                   (filter #(appropriate? goal %)
                           ops))))

(defn achieve-all [goals state ops]
  (every? (fn [goal]
           (not-empty (achieve goal state ops)))
         goals))

(defn gps [state goals ops]
  (when (achieve-all goals state ops)
    'solved))

(def school-ops
  (list
   (OP.  'drive-son-to-school
         #{'son-at-home 'car-works}
         #{'son-at-school}
         #{'son-at-home})
   (OP.  'shop-installs-battery
         #{'car-needs-battery 'shop-knows-problem 'shop-has-money}
         #{'car-works}
         #{})
   (OP.  'tell-shop-problem
         #{'in-communication-with-shop}
         #{'shop-knows-problem}
         #{})
   (OP.  'telephone-shop
         #{'know-phone-number}
         #{'in-communication-with-shop}
         #{})
   (OP.  'look-up-number
         #{'have-phone-book}
         #{'know-phone-number}
         #{})
   (OP.  'give-shop-money
         #{'have-money}
         #{'shop-has-money}
         #{'have-money})))

;;; Sample calls
;; cloj.gps1> (gps (set '(son-at-home car-needs-battery have-money have-phone-book))
;; 		(set '(son-at-school))
;; 		school-ops)
;; (executing give-shop-money)
;; (executing look-up-number)
;; (executing telephone-shop)
;; (executing tell-shop-problem)
;; (executing shop-installs-battery)
;; (executing drive-son-to-school)
;; solved

;; cloj.gps1> (gps (set '(son-at-home car-needs-battery have-money))
;; 		(set '(son-at-school))
;; 		school-ops)
;; (executing give-shop-money)
;; nil

;; cloj.gps1> (gps (set '(son-at-home car-works))
;; 		(set '(son-at-school))
;; 		school-ops)
;; (executing drive-son-to-school)
;; solved