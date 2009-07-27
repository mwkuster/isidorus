;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(defpackage :xml-tools
  (:use :cl :cxml)
  (:import-from :constants
		*xml-ns*
		*xmlns-ns*
		*rdf-ns*)
  (:export :get-attribute
           :xpath-fn-string
	   :xpath-child-elems-by-qname
	   :xpath-single-child-elem-by-qname
	   :xpath-select-location-path
	   :xpath-select-single-location-path
	   :get-ns-attribute
	   :clear-child-nodes
	   :absolute-uri-p
	   :get-node-name
	   :child-nodes-or-text
	   :get-xml-lang
	   :get-xml-base
	   :absolutize-value
	   :concatenate-uri
	   :push-string))

(in-package :xml-tools)

(defmacro push-string (obj place)
  "Imitates the push macro but instead of pushing object in a list,
   there will be appended the given string to the main string object."
  `(setf ,place (concatenate 'string ,place ,obj)))


(defun concatenate-uri (absolute-ns value)
  "Returns a string conctenated of the absolut namespace an the given value
   separated by either '#' or '/'."
  (declare (string absolute-ns value))
  (unless (or (> (length absolute-ns) 0)
	      (> (length value) 0))
    (error "From concatenate-uri(): absolute-ns and value must be of length > 0"))
  (unless (absolute-uri-p absolute-ns)
    (error "From concatenate-uri(): absolute-ns has to be an absolute URI: ~a" absolute-ns))
  (let ((last-char
	 (elt absolute-ns (- (length absolute-ns) 1))))
    (let ((separator
	   (cond
	     ((eql last-char #\#)
	      "#")
	     ((eql last-char #\/)
	      "/")
	     (t
	      "#")))
	  (prep-ns
	   (if (or (eql last-char #\#)
		   (eql last-char #\/))
	       (subseq absolute-ns 0 (- (length absolute-ns) 1))
	       absolute-ns)))
      (concatenate 'string prep-ns separator value))))


(defun absolutize-value(value base tm-id)
  "Returns the passed value as an absolute uri computed
   with the given base and tm-id."
  (declare (string value tm-id))
    (unless (absolute-uri-p tm-id)
      (error "From absolutize-value(): you must provide a stable identifier (PSI-style) for this TM: ~a" tm-id))
  (when (> (count #\# value) 2)
    (error "From absolutize-value(): value is allowed to have only one \"#\": ~a" value))
  (if (absolute-uri-p value)
      value
      (let ((prep-value
	     (if (> (length value) 0)
		 (string-left-trim "/" value)
		 ""))
	    (prep-base
	     (if (> (length base) 0)
		 (string-right-trim "/" base)
		 "")))
	(let ((fragment
	       (if (and (> (length prep-value) 0)
			(eql (elt prep-value 0) #\#))
		   (concatenate 'string prep-base prep-value)
		   (concatenate 'string prep-base "/" prep-value))))
	  (if (absolute-uri-p fragment)
	      fragment
	      (let ((prep-fragment
		     (when (> (length fragment) 0)
		       (string-left-trim "/" fragment)))
		    (prep-tm-id
		     (when (> (length tm-id) 0)
		       (string-right-trim "/" tm-id))))
		(concatenate 'string prep-tm-id "/" prep-fragment)))))))


(defun get-xml-lang(elem &key (old-lang nil))
  "Computes the current xml-lang attribute and returns
   its value as a string."
  (declare (dom:element elem))
  (let ((new-lang
	 (get-ns-attribute elem "lang" :ns-uri *xml-ns*)))
    (if (dom:has-attribute-ns elem *xml-ns* "lang")
	new-lang
	old-lang)))


(defun get-xml-base(elem &key (old-base nil))
  "Computes the current xml-base attribute and returns
   its value as a string."
  (declare (dom:element elem))
  (let ((new-base
	 (let ((inner-base
		(if (find #\# (get-ns-attribute elem "base" :ns-uri *xml-ns*))
		    (error "From get-xml-base(): the base-uri ~a is not valid"
			   (get-ns-attribute elem *xml-ns* "base"))
		    (get-ns-attribute elem "base" :ns-uri *xml-ns*))))
	   (if (and (> (length inner-base) 0)
		    (eql (elt inner-base 0) #\/))
	       (subseq inner-base 1 (length inner-base))
	       inner-base))))

    (if (or (absolute-uri-p new-base)
	    (not old-base))
	new-base
	(if (not new-base)
	    old-base
	    (concatenate 'string (string-right-trim "/" old-base)
			 "/" (string-left-trim "/" new-base))))))


(defun child-nodes-or-text (elem &key (trim nil))
  "Returns a list of dom-elements or a string.
   Is there only one child which is not a text node it will be
   returned as a list. Are only text nodes available their
   results are concatenated and returned as a string.
   comment nodes are removed anyway."
  (declare (dom:element elem))
  (let ((children
	 (remove-if #'(lambda(node)
			(when (dom:comment-p node)
			  t))
		    (dom:child-nodes elem)))
	(trim-fun (lambda(str)
		    (if trim
			(string-trim '(#\Space #\Tab #\Newline) str)
			str))))
    (if (find-if #'(lambda(node)
		     (unless (dom:text-node-p node)
		       t))
		 children)
	(remove-if #'(lambda(node)
		       (when (dom:text-node-p node)
			 (when (> (length
				   (string-trim '(#\Space #\Tab #\Newline)
						(dom:node-value node)))
				  0)
			   (error "Found literal content and xml-content in one node: ~a"
				  (dom:node-value node)))
			 t))
		   children)
	(let ((entire-string ""))
	  (map 'list #'(lambda(text-node)
			 (push-string (dom:node-value text-node) entire-string))
	       children)
	  (if (> (length (apply trim-fun (list entire-string))) 0)
	      (apply  trim-fun (list entire-string))
	      nil))))) ;there were no text nodes available


(defun absolute-uri-p (uri)
  "Returns t if the passed uri is an absolute one. This
   is indicated by a ':' with no leadgin '/'."
  (when uri
    (let ((position-of-colon
	   (position #\: uri)))
      (declare (string uri))
      (and position-of-colon (> position-of-colon 0)
	   (not (find #\/ (subseq uri 0 position-of-colon)))))))


(defun get-node-name (elem)
  "Returns the node's name without a prefix."
  (if (find #\: (dom:node-name elem))
      (subseq (dom:node-name elem)
	      (length (concatenate 'string (dom:prefix elem) ":")))
      (dom:node-name elem)))


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

(defun get-ns-attribute (elem name &key (ns-uri *rdf-ns*))
  "Returns athe attributes value. If the value is
   a string of the length 0, the return value is nil"
  (declare (dom:element elem))
  (declare (string ns-uri name))
  (let ((attr
	 (dom:get-attribute-ns elem ns-uri name)))
    (if (= (length attr) 0)
	nil
	attr)))


(defun clear-child-nodes (elem)
  "Returns a list of child nodes, where all text-nodes and
   all comment nodes are removed."
  (declare (dom:element elem))
  (loop for child-node across (dom:child-nodes elem)
     unless (or (dom:text-node-p child-node)
		(dom:comment-p child-node))
       collect child-node))