;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :jtm
  (:use :cl :json :datamodel :base-tools :isidorus-threading)
  (:export :import-from-jtm
	   :export-as-jtm
	   :export-as-jtm-string))

(in-package :jtm)

(defvar *jtm-xtm* "jtm-xtm"); Represents the currently active TM of the JTM-Importer

(defvar item_type-topicmap "topicmap")

(defvar item_type-topic "topic")

(defvar item_type-name "name")

(defvar item_type-variant "variant")

(defvar item_type-occurrence "occurrence")

(defvar item_type-association "association")

(defvar item_type-role "role")


(defun export-as-jtm-string (&key tm-id (revision (get-revision))
			     (jtm-format '1.1))
  "Exports a topic map or all stored constructs as JTM string.
   jtm-format must be set either to '1.0 or '1.1."
  (declare (type (or Null String) tm-id)
	   (Symbol jtm-format)
	   (type (or Null Integer) revision))
  (with-reader-lock
    (let ((tm 
	   (when tm-id
	     (get-item-by-item-identifier tm-id :revision revision)))
	  (version-1.1-p (eq jtm-format '1.1)))
      (let* ((tm-tops
	      (if tm
		  (delete-if #'(lambda(top)
				 (not (find-item-by-revision top revision)))
			     (topics tm))
		  (get-all-topics revision)))
	     (tm-assocs
	      (if tm
		  (delete-if #'(lambda(assoc)
				 (not (find-item-by-revision assoc revision)))
			     (associations tm))
		  (get-all-associations revision)))
	     (prefixes
	      (when version-1.1-p
		(create-prefix-list tm-tops tm-assocs tm :revision revision)))
	     (version (if version-1.1-p
			  "\"version\":\"1.1\","
			  "\"version\":\"1.0\","))
	     (prefix-value (when version-1.1-p
			      (concat "\"prefixes\":"
				      (export-prefix-list-to-jtm prefixes) ",")))
	     (iis (concat "\"item_identifiers\":"
			  (export-identifiers-to-jtm
			   tm :identifier-type 'ItemIdentifierC :prefixes prefixes
			   :revision revision) ","))
	     (topics (concat "\"topics\":"
			     (export-topics-to-jtm tm-tops :prefixes prefixes
						   :revision revision)))
	     (assocs (concat "\"associations\":"
			     (export-associations-to-jtm tm-assocs :prefixes prefixes
							 :revision revision)))
	     (item-type (concat "\"item_type\":" item_type-topicmap)))
	(concat "{" version prefix-value iis topics assocs item-type "}")))))
	     
	     

(defun export-as-jtm (jtm-path &key tm-id (revision (get-revision))
		      (jtm-format '1.1))
  "Exports a topic map or all stored constructs as JTM file by calling
   export-as-jtm-string."
  (declare (type (or Null String) jtm-path tm-id)
	   (Symbol jtm-format)
	   (type (or Null Integer) revision))
  (with-open-file (stream jtm-path :direction :output)
    (format stream (export-as-jtm-string :tm-id tm-id :revision revision
					 :jtm-format jtm-format))))


(defun create-prefix-list (topics associations topic-map &key revision)
  "Returns a list of the following structure: ((:pref 'pref_1'
   :value 'uri-pref') (...))."
  (declare (List topics associations)
	   (TopicMapC topic-map)
	   (type (or Null Integer)))
  (let ((identifiers
	 (append (loop for topic in topics
		    append
		      (append
		       (item-identifiers topic :revision revision)
		       (locators topic :revision revision)
		       (item-identifiers topic :revision revision)
		       (loop for name in (names topic :revision revision)
			  append (append
				  (item-identifiers name :revision revision)
				  (loop for variant in
				       (variants name :revision revision)
				     append (item-identifiers
					     variant :revision revision))))
		       (loop for occurrence in (occurrences topic :revision revision)
			  append (item-identifiers occurrence :revision revision))))
		 (loop for assoc in associations
		    append (append
			    (item-identifiers assoc :revision revision)
			    (loop for role in (roles assoc :revision revision)
			       append (item-identifiers role :revision revision))))
		 (when topic-map
		   (item-identifiers topic-map :revision revision)))))
    (let ((prefixes
	   (remove-duplicates
	    (remove-null (map 'list  #'(lambda(id)
					 (prefix-of-uri (uri id)))
			      identifiers)) :test #'string=)))
      (loop for idx to (length prefixes)
	 collect (list :pref (concat "pref_" (write-to-string (1+ idx)))
		       :value (elt prefixes idx))))))


(defun export-prefix-list-to-jtm (prefix-list)
  "Returns a json object that represent an object with namespaces and their
   prefix qualifiers."
  (declare (List prefix-list))
  (if prefix-list
      (let ((result "{"))
	(loop for item in prefix-list
	   do (push-string
	       (concat "\"" (getf item :pref) "\":"
		       (json:encode-json-to-string (getf item :value)) ",")
	       result))
	(concat (subseq result 0 (1- (length result))) "}"))
      "null"))