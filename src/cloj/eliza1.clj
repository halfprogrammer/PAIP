(ns cloj.eliza1)

(def fail nil)

(defn variable? [pattern]
  (and (symbol? pattern)
       (= \? (first (str pattern)))))

(defn match-variable
  "Does VAR match input? Updates and returns bindings"
  [var input bindings]
  (cond (not (bindings var)) (conj bindings {var input})
        (= input (bindings var)) bindings
        :else fail))

(defn starts-with? [lst x]
  "List starts with the given value"
  (and (list? lst)
       (= (first lst) x)))

(defn maplist
  "The mapping operation involves applying function to successive sets of arguments in which one argument is obtained from each collection."
  [f l & more-lsts]
  (let [all-lists (list* l more-lsts)
        seqs (map seq all-lists)
        rests (map rest all-lists)]
    (lazy-seq
     (when-not (some nil? seqs)
       (cons (apply f all-lists)
              (apply maplist f rests))))))

(defn segment-pattern?
  "Is this a segment matching pattern ((?* var) . pat)"
  [pattern]
  (and (list? pattern)
       (starts-with? (first pattern) '?*)))

(declare pat-match)

(defn sublist [lst start end]
  (first (split-at (dec end) (second (split-at start lst)))))

(defn segment-match
  "Match the segment pattern ((?* var) . pat) against the input."
  [pattern input bindings]
  (let [var (second (first pattern))
        pat (next pattern)]
    (if (nil? pat)
      (match-variable var input bindings)
      (if-let [matching-subseqs (seq (filter #(starts-with? (second %) (first pat))
                                             (maplist vector
                                                      (range 0 (count input))
                                                      input)))]
        (some (fn [[[pos] match]]
                (let [matches (match-variable var (sublist input 0 (inc pos)) bindings)]
                  (pat-match pat match matches)))
              matching-subseqs)
        fail))))

(defn pat-match
  "Match pattern against input in the context of the bindings."
  ([pattern input bindings]
     (cond (= bindings fail) fail
           (variable? pattern) (match-variable pattern input bindings)
           (= pattern input) bindings
           (segment-pattern? pattern) (segment-match pattern input bindings)
           (and (coll? pattern) (coll? input)) (recur (rest pattern)
                                                      (rest input)
                                                      (pat-match (first pattern)
                                                                 (first input)
                                                                 bindings))
           :else fail))
  ([pattern input]
     (pat-match pattern input {})))

(defonce eliza-rules
  {'((?* ?x) hello (?* ?y))
   '((How do you do. Please state your problem.))

   '((?* ?x) I want (?* ?y))
   '((What would it mean if you got ?y)
     (Why do you want ?y)
     (Suppose you got ?y soon))

   '((?* ?x) if (?* ?y))
   '((Do you really think it is likely that ?y)
     (Do you with that ?y)
     (What do you think about ?y)
     (Really -- if ?y))

   '((?* ?x) no (?* ?y))
   '((Why not?)
     (You are being negative)
     (Are you saying "no" just to be negative?))

   '((?* ?x) I was (?* ?y))
   '((Were you really?)
     (Perhaps I already knew you were ?y)
     (Why do you tell me you were ?y now?))

   '((?* ?x) I feel (?* ?y))
   '((Do you often feel ?y ?))

   '((?* ?x) I felt (?* ?y))
   '((What other feelings do you have?))})

(defn rule-pattern [rule]
  (key rule))

(defn rule-responses [rule]
  (val rule))

(defn switch-viewpoint [bindings]
  (reduce (fn [replaced b]
            (let [k (key b)
                  v (val b)]
              (merge replaced
                     {k (replace {'I 'you, 'you 'I, 'me 'you, 'am 'are} v)})))
          {}
          bindings))

(defn use-eliza-rules
  "Find some rule with which to transform the input."
  [input rules]
  (some (fn [rule]
          (let [pattern (rule-pattern rule)
                responses (rule-responses rule)
                result (pat-match pattern input)]
            (when (not= result fail)
              (replace (switch-viewpoint result)
                       (rand-nth responses)))))
        rules))

(defn eliza
  "Respond to user input using pattern matching rules."
  []
  (print 'eliza>)
  (println (flatten (use-eliza-rules (read) eliza-rules)))
  (recur))