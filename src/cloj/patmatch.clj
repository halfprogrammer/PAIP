(ns cloj.patmatch)

(def fail 'nil)

(declare variable? match-variable
         segment-pattern? segment-matcher
         single-pattern? single-matcher)

(defn pat-match
  "Match pattern against input in the current context of the bindings"
  ([pattern input bindings]
     (cond
      (= bindings fail) fail
      (variable? pattern) (match-variable pattern input bindings)
      (= input pattern) bindings
      (segment-pattern? pattern) (segment-matcher pattern input bindings)
      (single-pattern? pattern) (single-matcher pattern input bindings)
      (and (list? pattern) (list? input)) (recur (rest pattern)
                                                 (rest input)
                                                 (pat-match (first pattern)
                                                            (first input)
                                                            bindings))
      :else fail))
  
  ([pattern input]
     (pat-match pattern input {})))

(defn variable?
  "Is x a variable (a symbol starting with ?)"
  [x]
  (and (symbol? x) (= \? (first (str x)))))

(defn match-variable
  "Does VAR match input? Updates and returns bindings"
  [var input bindings]
  (cond (not (bindings var)) (conj bindings {var input})
        (= input (bindings var)) bindings
        :else fail))

