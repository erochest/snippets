
;;;; This is from http://clj-me.blogspot.com/2008/05/jumping-to-javadocs-from-repl.html

(in-ns 'javadoc)
(clojure/refer 'clojure)

(defn open-url [url]
  (let [htmlpane (new javax.swing.JEditorPane url)]
    (.setEditable htmlpane false)
    (.addHyperlinkListener htmlpane
      (proxy [javax.swing.event.HyperlinkListener] []
        (hyperlinkUpdate [#^javax.swing.event.HyperlinkEvent e]
          (when (= (.getEventType e) (. javax.swing.event.HyperlinkEvent$EventType ACTIVATED))
            (if (instance? javax.swing.text.html.HTMLFrameHyperlinkEvent e)
              (.. htmlpane getDocument (processHTMLFrameHyperlinkEvent e))
              (try
                (.setPage htmlpane (.getURL e))
                (catch Throwable t
                               (.printStackTrace t))))))))
    (doto (new javax.swing.JFrame)
      (setContentPane (new javax.swing.JScrollPane htmlpane))
      (setBounds 32 32 700 900)
      (show))))

(defn javadoc [c]
  (let [url (str "http://java.sun.com/javase/6/docs/api/"
              (.. c getName (replace \. \/) (replace \$ \.)) ".html")]
    (open-url url)))

(comment
  ;;; usage:
  (javadoc Throwable) 
  ;;opens a window displaying Throwable's javadoc 

  ;;; hint:
  (javadoc (class some-object))
  )

