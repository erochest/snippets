
;;;; This is code written while perusing http://11011110.livejournal.com/135302.html

(in-ns 'dag)
(clojure/refer 'clojure)

;;; General DAG functions

;; A DAG is a hash-map, with each node pointing to a list of  nodes.

(defn add-edges
  "This adds the edges -- pairs of nodes -- to the graph:
    (add-edges dag :a :b :a :c :b :c)"
  ([dag]
   dag)
  ([dag src dest]
   (assoc dag src (conj (get dag src) dest)))
  ([dag src dest & edges]
   (let [dag-2 (apply add-edges dag edges)]
     (assoc dag-2 src (conj (get dag-2 src) dest)))))

(defn make-graph
  "This creates a graph, building it from pairs of node identifiers."
  ([]
   {})
  ([& edges]
   (apply add-edges {} edges)))

(defn get-edges
  "This returns the edges from a given node."
  [dag node]
  (get dag node))

(defn print-dag
  "This prints the DAG to *out*."
  [dag]
  (loop [nodes (sort (keys dag))]
    (when nodes
      (println (first nodes) '=> (get-edges dag (first nodes)))
      (recur (rest nodes)))))

(defn- vector-contains?
  "This returns the index of the item in the vector, or false if it is not
  found."
  [vec item]
  (loop [n 0]
    (cond (== n (count vec)) false
          (= (nth vec n) item) n
          :else (recur (inc n)))))

(defstruct #^{:private true} sort-state :seen :working :in-order)

(defn sort-graph
  "This performs a reverse topological sort on a DAG."
  [dag]
  (let [
        sg (fn sg [state node]
             (assert (not (first (filter #(= % node) (:working state)))))
             (if (contains? (:seen state) node)
               state
               (let [edged (reduce sg
                                   (assoc state :working (conj (:working state) node))
                                   (get-edges dag node))]
                 (assoc edged
                        :seen (conj (:seen edged) node)
                        :in-order (conj (:in-order edged) node)
                        :working (pop (:working edged))))))
        ]
    (loop [nodes (keys dag) state (struct sort-state #{} [] [])]
      (if nodes
        (recur (rest nodes) (sg state (first nodes)))
        (:in-order state)))))

(defn get-root
  "This finds the root node by sorting the graph with sort-graph and returning
  the last item in the list."
  [dag]
  (first (reverse (sort-graph dag))))

;;; Breadth first search
;;; http://en.wikipedia.org/wiki/Breadth-first_search

(defn breadth-first
  "This searchs a DAG for nodes matching a given predicate."
  ([dag pred]
   (breadth-first dag pred (get-root dag)))
  ([dag pred start]
   (loop [q (list (list start)), seen #{}]
     (if (nil? q)
       nil
       (let [[node & path :as next-path] (first q)]
         (println "searching" next-path) (flush)
         (cond (seen node) (recur (rest q) seen)
               (pred node) (reverse next-path)
               :else (recur (concat (rest q)
                                    (map #(cons % next-path)
                                         (filter (complement seen) (get-edges dag node))))
                            (conj seen node))))))))

;;; Dijkstra's Algorithm
;;; http://en.wikipedia.org/wiki/Dijkstra%27s_algorithm

;;; A*
;;; http://en.wikipedia.org/wiki/A%2A_search_algorithm

;;; Bellman-Ford
;;; http://en.wikipedia.org/wiki/Bellman-Ford_algorithm

