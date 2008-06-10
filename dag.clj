
;;;; This is code written while perusing http://11011110.livejournal.com/135302.html

(in-ns 'dag)
(clojure/refer 'clojure)

;;; General DAG functions

;; A DAG is a hash-map of dag-node structs. Each dag-node has an ID number (its
;; index in the DAG vector), a data payload, and a hash-map of edges, mapping
;; the edge nodes' IDs to their weightings.

(defstruct dag-node :id :data :edges)
(defstruct dag-edge :src :dest :weight)

(defn add-edges
  "This adds the edges -- vector pairs of a source node, a destination node,
  and a weighting -- to the graph:
  (add-edges dag :a :b 1.0, :a :c 1.0, :b :c 0.5)"
  ([dag]
   dag)
  ([dag src dest weight]
   (let [src-node (get dag src)
         edges (assoc (:edges src-node) dest weight)]
     (assoc dag src (if src-node
                      (assoc src-node :edges edges)
                      (struct dag-node src nil edges)))))
  ([dag src dest weight & edges]
   (add-edges (apply add-edges dag edges) src dest weight)))

(defn make-graph
  "This creates a graph, building it from pairs of node identifiers."
  ([]
   {})
  ([& edges]
   (apply add-edges (make-graph) edges)))

(defn get-edges
  "This returns the edges from a given node."
  [dag node]
  (-> dag (get node) :edges keys))

(defn get-edge-weight
  "This returns the weighting for a given edge. If the edge doesn't exist, it
  returns 0.0."
  [dag src dest]
  (-> dag (get src) :edges (get dest 0.0)))

(defn get-data
  "This returns the data payload for the node."
  [dag node]
  (-> dag (get node) :data))

(defn print-dag
  "This prints the DAG to *out*."
  [dag]
  (let [
        print-edges (fn print-edges [dests edges]
                      (when dests
                        (let [dest (first dests)
                              weight (get edges dest)]
                          (println (str "    => " dest " (" weight ")"))
                          (recur (rest dests) edges))))
        ]
    (loop [nodes (sort (keys dag))]
      (when nodes
        (let [node (get dag (first nodes))]
          (print (:id node))
          (when-let data (:data node)
            (print '/ data))
          (println)
          (print-edges (sort (keys (:edges node))) (:edges node))
          (recur (rest nodes)))))))

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
               (pred (get dag node)) (reverse next-path)
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

