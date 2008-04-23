
;;;; This is a close translation of the "Getting Started" code on 
;;;; http://www.zentus.com/sqlitejdbc/ to Clojure. The point is to illustrate
;;;; how to call SQLite from Clojure.

(import '(java.sql DriverManager))

(. Class forName "org.sqlite.JDBC")

(def cxn (. DriverManager getConnection "jdbc:sqlite:test.db"))
(def stmt (. cxn createStatement))

(. stmt executeUpdate "CREATE TABLE people (name, occupation);")

(def prep (. cxn prepareStatement "INSERT INTO people VALUES (?, ?);"))
(doto prep
  (setString 1 "Gandhi")
  (setString 2 "politics")
  (addBatch)
  (setString 1 "Turing")
  (setString 2 "computers")
  (addBatch)
  (setString 1 "Wittgenstein")
  (setString 2 "smartypants")
  (addBatch))

(. cxn setAutoCommit false)
(. prep executeBatch)
(. cxn setAutoCommit true)

(defn dump-people [rs]
  (if (. rs next)
    (do
      (println "name =" (. rs getString "name"))
      (println "job  =" (. rs getString "occupation"))
      (recur rs))
    (. rs close)))

(dump-people (. stmt executeQuery "SELECT * FROM people;"))
(. cxn close)

