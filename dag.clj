
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

(defn dijkstra
  "This is a probably-naive implementation of Dijkstra's algorithm in Clojure.
  It returns a vector of two items. The first is a mapping of nodes to their
  shortest distance to the start, and the second is a mapping of nodes to their
  immediate parent in the shortest path."
  ([dag]
   (dijkstra dag (get-root dag)))
  ([dag start]
   (dijkstra dag start nil))
  ([dag start end]
   (let [get-next (fn get-next
                    "Return the node closest to the start that has not been
                    processed yet."
                    [seen dist]
                    (when-let unseen (filter (complement seen) (keys dist))
                      (reduce (partial min-key (partial get dist)) unseen)))]
     ; 1. Create a distance list, a previous vertex list, and a current vertex
     ; 2. All the values in the distance list are set to infinity except the starting
     ;    vertex which is set to zero
     ; 3. All values in visited list are set to false
     ; 4. All values in the previous list are set to a special value signifying that they
     ;    are undefined, such as null
     (loop [dist (assoc (zipmap (keys dag) (cycle (list (. Integer MAX_VALUE)))) start 0)
            previous {}
            seen #{}]
       ; 5. Current vertex is set as the starting vertex
       ; 8. Update the current vertex to the unvisited vertex that can be reached by
       ;    the shortest path from the starting vertex
       ; 9. Repeat from step 6 until all nodes are visited
       ; 6. Mark the current vertex as visisted
       (let [next (get-next seen dist)]
         (cond (and next (= next end)) [dist previous]
               next
               (let [next-dist (get dist next)
                     seen+next (conj seen next)
                     edges (filter (complement (comp seen+next key)) (-> dag (get next) :edges))]
                 ; 7. Update distance and previous lists based on those vertices which can be
                 ;    immediately reached from the current vertex
                 (recur (reduce (fn [m e] (assoc m (key e) (+ (val e) next-dist))) dist edges)
                        (reduce (fn [m e] (assoc m (key e) next)) previous edges)
                        seen+next))
               :else [dist previous]))))))

;;; A*
;;; http://en.wikipedia.org/wiki/A%2A_search_algorithm

;;; Bellman-Ford
;;; http://en.wikipedia.org/wiki/Bellman-Ford_algorithm

