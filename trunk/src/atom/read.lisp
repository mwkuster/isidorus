(in-package :atom)

(defmacro parse-feed ((fragment-feed-string) &body make-entry)
  "a convenience macro that captures key parsing elements for
feeds. As body it takes the action to be performed on each entry in
the feed (usually a register-entry statement)"
  `(let*
      ((feed-dom
        (dom:document-element
         (cxml:parse-rod ,fragment-feed-string (cxml-dom:make-dom-builder))))
       (feed 
        (make-instance 'fragments-feed 
                       :id  (xpath-fn-string 
                                        (xpath-single-child-elem-by-qname
                                          feed-dom
                                          *atom-ns* "id"))
                       ;;TODO: verify if that is a good idea
                       :tm-id (xpath-fn-string 
                                        (xpath-single-child-elem-by-qname
                                          feed-dom
                                          *atom-ns* "id"))
                       :link
                       (get-attribute 
                        (xpath-single-child-elem-by-qname feed-dom *atom-ns* "link")
                        "href")
                       :source-locator-prefix (xpath-fn-string 
                                        (xpath-single-child-elem-by-qname
                                          feed-dom
                                          *egovpt-ns* "ServerSrcLocatorPrefix")))))
    (loop for entry-elem across
        (xpath-child-elems-by-qname feed-dom
                                    *atom-ns* "entry")
         do
         ,@make-entry)
    feed))


(defun parse-fragments-feed (fragment-feed-string)
  (parse-feed (fragment-feed-string)
    (register-entry 
     feed 
     (make-instance 'fragment-entry
                    :id (xpath-fn-string
                         (xpath-single-child-elem-by-qname entry-elem *atom-ns* "id"))
                    :link 
                    (get-attribute
                     (xpath-single-child-elem-by-qname entry-elem *atom-ns* "link")
                     "href")
                    :psi
                    (xpath-fn-string
                     (xpath-single-child-elem-by-qname entry-elem *egovpt-ns* "TopicSI"))))))

(defun parse-snapshots-feed (fragment-feed-string)
  (parse-feed (fragment-feed-string)
    (register-entry 
     feed 
     (make-instance 'snapshot-entry
                    :id (xpath-fn-string
                         (xpath-single-child-elem-by-qname entry-elem *atom-ns* "id"))
                    :link 
                    (get-attribute
                     (xpath-single-child-elem-by-qname entry-elem *atom-ns* "link")
                     "href")))))
             
    