
;;;;
;;;; This illustrates the Rozenshtein Method for implementing a
;;;; cross-tab query, which moves rows into columns.
;;;;
;;;; This is taken from http://www.stephenforte.net/PermaLink.aspx?guid=2b0532fc-4318-4ac0-a405-15d6d813eeb8
;;;;

;;; This connects to the database.

(asdf:oos 'asdf:load-op :clsql-sqlite3)

(in-package :cl-user)

(defpackage :roz
  (:use :clsql :cl))

(in-package :roz)

(locally-enable-sql-reader-syntax)

(connect '(":memory:") :database-type :sqlite3)

(defvar *initial-data*
  '(("widget" 42 1) ("widget" 42 1) ("widget" 42 1)
    ("widget" 42 2) ("widget" 42 2) ("widget" 42 2) ("widget" 42 2)
    ("widget" 42 3) ("widget" 42 3)
    ("widget" 42 4)))

;;; This creates the tables and sets up the data.

(defun create-customers ()
  (create-table [customers]
                '(([item] text)
                  ([unit-price] integer)
                  ([order-month] integer))))

(defun populate-customers ()
  (loop :for row :in *initial-data*
     :do (insert-records :into [customers]
                         :attributes '([item] [unit-price] [order-month])
                         :values row)))

;;; What we want is something that returns the query:
;;;   what   total jan feb mar apr ...
;;;   widget 10    3   4   2   1

(defun pivot-query ()
  (select [item] [count [*] ]               ; total
          [sum [= [- [order-month] 1] 0] ]  ; jan
          [sum [= [- [order-month] 2] 0] ]  ; feb
          [sum [= [- [order-month] 3] 0] ]  ; mar
          [sum [= [- [order-month] 4] 0] ]  ; apr
                                        ; ....
          :from [customers]))

;;; Another variation is below, which is clipped from the original article:
;SELECT  CompanyName, SUM((UnitPrice*Quantity)) As TotalAmt,
;SUM((UnitPrice*Quantity)*(1-ABS(SIGN(DatePart(mm,OrderDate)-1)))) AS Jan, 
;SUM((UnitPrice*Quantity)*(1-ABS(SIGN(DatePart(mm,OrderDate)-2)))) AS Feb, 
;SUM((UnitPrice*Quantity)*(1-ABS(SIGN(DatePart(mm,OrderDate)-3)))) AS Mar, 
;SUM((UnitPrice*Quantity)*(1-ABS(SIGN(DatePart(mm,OrderDate)-4)))) AS Apr, 
;SUM((UnitPrice*Quantity)*(1-ABS(SIGN(DatePart(mm,OrderDate)-5)))) AS May, 
;SUM((UnitPrice*Quantity)*(1-ABS(SIGN(DatePart(mm,OrderDate)-6)))) AS Jun, 
;SUM((UnitPrice*Quantity)*(1-ABS(SIGN(DatePart(mm,OrderDate)-7)))) AS Jul, 
;SUM((UnitPrice*Quantity)*(1-ABS(SIGN(DatePart(mm,OrderDate)-8)))) AS Aug, 
;SUM((UnitPrice*Quantity)*(1-ABS(SIGN(DatePart(mm,OrderDate)-9)))) AS Sep, 
;SUM((UnitPrice*Quantity)*(1-ABS(SIGN(DatePart(mm,OrderDate)-10)))) AS Oct, 
;SUM((UnitPrice*Quantity)*(1-ABS(SIGN(DatePart(mm,OrderDate)-11)))) AS Nov, 
;SUM((UnitPrice*Quantity)*(1-ABS(SIGN(DatePart(mm,OrderDate)-12)))) AS Dec
;FROM         Customers INNER JOIN
;                Orders ON Customers.CustomerID = Orders.CustomerID INNER JOIN
;                [Order Details] ON Orders.OrderID = [Order Details].OrderID
;             Group By Customers.CompanyName

