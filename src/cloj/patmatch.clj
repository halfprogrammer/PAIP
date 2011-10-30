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


(declare match-is match-or match-and match-not)

(def single-match-table
  {'?is match-is
   '?or match-or
   '?and match-and
   '?not match-not})

(declare segment-match segment-match+ segment-match? match-if)

(def segment-match-table
  {'?* segment-match
   '?+ segment-match+
   '?? segment-match?
   '?if match-if})

(declare segment-match-fn single-match-fn)

(defn segment-pattern?
  "Is this a segment-matching pattern like ((?* var) . pat)"
  [pattern]
  (and (list? pattern) (list? (first pattern))
       (symbol? (ffirst pattern))
       (segment-match-fn (ffirst pattern))))

(defn single-pattern?
  "Is this a single-matching pattern?
Eg. (?is x predicate) (?and predicate) (?or predicate)."
  [pattern]
  (and (list? pattern)
       (single-match-fn (first pattern))))

(defn segment-matcher
  "Call the right function for this kind of segment matcher"
  [pattern input bindings]
  ;; (println "smf input:" (rest pattern) input bindings)
  ((segment-match-fn (ffirst pattern)) pattern input bindings))

(defn single-matcher
  "Call the right function for this kind of single matcher"
  [pattern input bindings]
  ((single-match-fn (first pattern)) (rest pattern) input bindings))

(defn segment-match-fn
  "Get the segment-match function for x if it has one."
  [x]
  (segment-match-table x))

(defn single-match-fn
  "Get the single-match function for x if it has one."
  [x]
  (single-match-table x))

(defn match-is
  "Succeed and bind var if the input satisfies pred,
where var-and-pred is the list (var pred)"
  [var-and-pred input bindings]
  (let [[var pred] var-and-pred
        new-bindings (pat-match var input bindings)]
    (if (or (= new-bindings fail)
            (not (pred input)))
      fail
      new-bindings)))

(defn match-and
  "Succeed if all the patterns match the input."
  [patterns input bindings]
  (cond (= bindings fail) fail
        (empty? patterns) bindings
        :else (match-and (rest patterns) input
                         (pat-match (first patterns) input bindings))))

(defn match-or
  "Succeed if any one of the patterns match the input."
  [patterns input bindings]
  (if (empty? patterns)
    fail
    (let [new-bindings (pat-match (first patterns)
                                  input bindings)]
      (if (= new-bindings fail)
        (match-or (rest patterns) input bindings)
        new-bindings))))

(defn match-not
  "Success if none of the patterns match the input. This will not bind anything"
  [patterns input bindings]
  (if (match-or patterns input bindings)
    fail
    bindings))

(defn maplist
  "The mapping operation involves applying function to successive sets of arguments in which one argument is obtained from each collection."
  [f l & more-lsts]
  (let [all-lists (list* l more-lsts)
        seqs (map seq all-lists)
        rests (map rest all-lists)]
    ;; (println "all-lists:" all-lists)
    ;; (println "seqs:" seqs)
    ;; (println "rests:" rests)
    (lazy-seq
     (when-not (some nil? seqs)
       (cons (apply f all-lists)
              (apply maplist f rests))))))

(defn starts-with? [lst x]
  "List starts with the given value"
  (and (list? lst)
       (= (first lst) x)))

(defn find-position-and-sublist
  "Given a list and a element to look for it returns a vector [idx lst]
where idx is the index of the element in the list and lst is the sublist"
  [lst x]
  (let [idx-sublists (map list (range)
                          (maplist identity lst))
        sublist-starts-with (filter #(starts-with? (second %) x) idx-sublists)]
    sublist-starts-with))

(defn find-match-pos
  "Find the matching position of pat1 in the input. Pat1 should be a constant."
  [pat1 input]
  (when-not (or (coll? pat1) (variable? pat1))
    (find-position-and-sublist input pat1)))

(defn segment-match
  "Match the segment pattern ((?* var) . pat) against the input"
  [pattern input bindings]
  (let [var (second (first pattern))
        pat (next pattern)]
    (println "var:" var)
    (println "pat:" pat)
    (if (nil? pat)
      (match-variable var input bindings)
      (do
        (println "find-match-pos:"(find-match-pos (first pat) input))
        (some (fn [[idx lst]]
                (let [res (pat-match pat
                                     (drop idx input)
                                     (match-variable var (take idx input) bindings))]
                  (when-not (= res fail)
                    res)))
              (find-match-pos (first pat) input))))))

