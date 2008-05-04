
;;;; This is a close translation of the "Getting Started" code on 
;;;; http://www.zentus.com/sqlitejdbc/ to Clojure. The point is to illustrate
;;;; how to call SQLite from Clojure.

(import '(java.sql DriverManager))
(import '(java.lang Double Integer))

(. Class forName "org.sqlite.JDBC")

(defmulti set-sql-param (fn [stmt n value] (. value getClass)))

(defmethod set-sql-param :default [stmt n value]
  (. stmt setString n (str value)))

(defmethod set-sql-param Double [stmt n value]
  (. stmt setDouble n value))

(defmethod set-sql-param Integer [stmt n value]
  (. stmt setInteger n value))

(defn do-batch [stmt & data]
  (let [
        add-row (fn add-row [n row]
                  (if row
                    (do
                      (set-sql-param stmt n (first row))
                      (recur (inc n) (rest row)))
                    (. stmt addBatch)))
        ]
    (loop [data data]
      (if data
        (do
          (add-row 1 (seq (first data)))
          (recur (rest data)))
        (. stmt executeBatch)))))

(defn dump-people [rs]
  (when (. rs next)
    (println "name =" (. rs getString "name"))
    (println "job  =" (. rs getString "occupation"))
    (recur rs)))

(with-open cxn (. DriverManager getConnection "jdbc:sqlite:test.db")
  (let [stmt (. cxn createStatement)]
    (. stmt executeUpdate "CREATE TABLE people (name, occupation);")
    (. cxn setAutoCommit false)
    (do-batch (. cxn prepareStatement "INSERT INTO people VALUES (?, ?);")
              ["Gandhi" "politics"] ["Turing" "computers"] ["Wittgenstein" "smartypants"])
    (. cxn setAutoCommit true)
    (with-open rs (. stmt executeQuery "SELECT * FROM people;")
      (dump-people rs))))

