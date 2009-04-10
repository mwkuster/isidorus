;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(defpackage :xml-tools
  (:use :cl :cxml)
  (:export :get-attribute
           :xpath-fn-string
	   :xpath-child-elems-by-qname
	   :xpath-single-child-elem-by-qname
	   :xpath-select-location-path
	   :xpath-select-single-location-path))

(in-package :xml-tools)

(defun conditional-fn (fn b)
  (if b 
      fn
      (lambda(x) (list x))
      )
  )

(defun handle-whitespace (strip-whitespace s)
    (cond
      (strip-whitespace
       (string-trim '(#\Space #\Tab #\Newline) s))
      (t s)))

(defun xpath-fn-string (elem &optional (strip-whitespace t))
  "Extract the string value of an XML DOM element (with subelements)"
  (declare (dom:element elem))
  ;;  ((conditional-fn #'(lambda(s) (string-trim " #\t#\n" s)) strip-whitespace ; 
  (handle-whitespace strip-whitespace
   (apply #'concatenate 'string 
               (map 'list
                    (lambda (s)
                      (cond
                        ((dom:text-node-p s)
                         (dom:node-value s))
                        ((dom:element-p s)
                         (xpath-fn-string s))))
                    (dom:child-nodes elem)))))

(defun attr-value (attr)
  (dom:node-value attr))


(defun has-qname (elem namespace-uri local-name)
  (declare (dom:node elem))
  (and 
   (dom:element-p elem)
   (equal (dom:local-name elem) local-name)
   (equal (dom:namespace-uri elem) namespace-uri))
  )


(defun xpath-child-elems-by-qname (elem namespace-uri local-name)
  "Returns a vector(!) with all the child elements of elem that have the 
qname '(namespace-uri local-name)"
  (declare (dom:element elem))
  (remove-if-not (lambda (el) (has-qname el namespace-uri local-name)) (dom:child-nodes elem))
  )


(defun xpath-single-child-elem-by-qname (elem namespace-uri local-name)
  "Returns some child of elem that has qname (namespace-uri local-name) or
nil if no such child exists."
  (declare (dom:element elem))
  (find-if (lambda (el) (has-qname el namespace-uri local-name)) (dom:child-nodes elem))
  )


(defun string-starts-with (begin str)
  (equal (char str 0) begin))


(defun xpath-select-location-path (elem list-of-qnames)
  "Takes a list of qnames (expressed as pairs of namespace-uris and
local names) and evaluates this as a location path" 
  (declare (dom:element elem))
  (let
      ((namespace-uri (eval (first (first list-of-qnames))))
       (local-name (eval (second (first list-of-qnames))))
       (rest-of-qnames (rest list-of-qnames)))
    (cond
      (list-of-qnames 
       (cond
         ((string-starts-with #\@ local-name)
          (list (dom:get-attribute-node-ns elem namespace-uri (string-left-trim "@" local-name))))
         (t
          (apply #'append 
              (map 'list 
                   (lambda (child) 
                     (xpath-select-location-path child rest-of-qnames))
                   (xpath-child-elems-by-qname elem namespace-uri local-name))
              ))))
      (t (list elem)))))


(defun xpath-select-single-location-path (elem list-of-qnames)
  "Takes a list of qnames (expressed as pairs of namespace-uris and
local names) and evaluates this as a location path; returns one element that
satisfies this location path or nil if there is no such element." 
  (first (xpath-select-location-path elem list-of-qnames)) 
  ;; TODO: Optimize - it is inefficient to first construct the full list and then
  ;; to throw away all but the first element!
)


(defun attributes-to-strings (attrs)
  (map 'list #'attr-value attrs)
)

(defun get-attribute (elem attrname)
  "Returns the value of the attribute attrname of element elem. If
elem is nil or does not have the attribut attrname, the function
returns nil"
  (when elem
    (let 
        ((attr-node
          (dom:get-attribute-node elem attrname)))
      (when attr-node
        (dom:node-value attr-node)))))

;;(defvar top (elt *topic-list* 501))
;;(defvar scopes (xpath-select-location-path top '((*xtm-ns* "baseName") (*xtm-ns* "scope"))))

