(ns cloj.patmatch)

(use '[evalive.core :only (evil destro)])

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
      (and (seq? pattern) (seq? input)) (recur (rest pattern)
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
  {
   '?is match-is
   '?or match-or
   '?and match-and
   '?not match-not
   })

(declare segment-match segment-match+ segment-match? match-if)

(def segment-match-table
  {
   '?* segment-match
   '?+ segment-match+
   '?? segment-match?
   '?if match-if
   })

(declare segment-match-fn single-match-fn)

(defn segment-pattern?
  "Is this a segment-matching pattern like ((?* var) . pat)"
  [pattern]
  (and (seq? pattern) (seq? (first pattern))
       (symbol? (ffirst pattern))
       (segment-match-fn (ffirst pattern))))

(defn single-pattern?
  "Is this a single-matching pattern?
Eg. (?is x predicate) (?and predicate) (?or predicate)."
  [pattern]
  (and (seq? pattern)
       (single-match-fn (first pattern))))

(defn segment-matcher
  "Call the right function for this kind of segment matcher"
  [pattern input bindings]
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
    (lazy-seq
     (when-not (some nil? seqs)
       (cons (apply f all-lists)
              (apply maplist f rests))))))

(defn starts-with? [lst x]
  "List starts with the given value"
  (and (seq? lst)
       (= (first lst) x)))

(defn sublist
  ([lst start]
     (drop start lst))
  ([lst start end]
     (drop start (take end lst))))

(defn find-position-and-sublist
  "Given a list and a element to look for it returns a vector [idx lst]
where idx is the index of the element in the list and lst is the sublist"
  [lst x]
  (let [idx-sublists (map list (range)
                          (maplist identity lst))
        sublist-starts-with (filter #(starts-with? (second %) x) idx-sublists)]
    sublist-starts-with))

(def atom? (complement coll?))

(defn get-index
  "Find the index of the key starting from start"
  [lst key & {:keys [start] :or {start 0}}]
  (first (filter (complement nil?) (map (fn [elt idx]
                                          (when (= elt key)
                                            idx))
                                        (nthnext lst start)
                                        (range)))))

(defn first-match-pos
  "Find the matching position of pat1 in the input. Pat1 should be a constant."
  [pat1 input start]
  (cond (and (atom? pat1) (not (variable? pat1))) (get-index input pat1 :start start)
        (< start (count input)) start
        :else nil))

(defn segment-match
  "Match the segment pattern ((?* var) . pat) against the input"
  ([pattern input bindings]
     (segment-match pattern input bindings 0))
  
  ([pattern input bindings start]
     (let [var (second (first pattern))
           pat (next pattern)]
       (if (nil? pat)
         (match-variable var input bindings)
         (let [pos (first-match-pos (first pat) input start)]
           (if (nil? pos)
             fail
             (let [mv (match-variable var (sublist input 0 pos) bindings)
                   b2 (pat-match pat (sublist input pos) mv)]
               (if (= b2 fail)
                 (segment-match pattern input bindings (inc pos))
                 b2))))))))

(defn segment-match+
  "Match one or more elements"
  [pattern input bindings]
  (segment-match pattern input bindings 1))

(defn segment-match?
  "Match zero or one element"
  [pattern input bindings]
  (let [var (second (first pattern))
        pat (next pattern)]
    (or (pat-match (list* var pat) input bindings)
        (pat-match pat input bindings))))

(defn match-if
  "Test an arbitrary expression involving variables"
  [pattern input bindings]
  (let [con (evalive.core/lexical-context)]
    (create-ns 'abcd)
    (doseq [kv (con 'bindings)]
      (intern 'abcd (key kv) (val kv)))
    (ns abcd)
    (intern 'abcd 'res (eval (second (first pattern)))))
  (ns cloj.patmatch)
  (and (deref (ns-resolve 'abcd 'res))
       (pat-match (rest pattern) input bindings)))

;;; This works!
;; (pat-match '(?x ?op ?y is ?z (?if (= ((resolve ?op) ?x ?y) ?z)))
;; 			  '(3 + 4 is 7))