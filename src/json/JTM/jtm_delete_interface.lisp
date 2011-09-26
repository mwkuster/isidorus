;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :jtm-delete-interface
  (:use :cl :datamodel :jtm)
  (:export :mark-as-deleted-from-jtm))

(in-package :jtm-delete-interface)

(defun mark-as-deleted-from-jtm (jtm-data &key (revision *TM-REVISION*))
  "Marks an object that is specified by the given JSON data as deleted."
  (declare (string jtm-data) (integer revision))
  (let ((json-list (json:decode-json-from-string jtm-data)))
    (let ((type nil)
	  (parent nil)
	  (parent-of-parent nil)
	  (delete nil))
      (loop for json-entry in json-list
	 do (let ((st (car json-entry))
		  (nd (cdr json-entry)))
	      (cond ((eql st :type)
		     (setf type nd))
		    ((eql st :delete)
		     (setf delete nd))
		    ((eql st :parent)
		     (setf parent nd))
		    ((eql st :parent-of-parent)
		     (setf parent-of-parent nd)))))
      (cond ((string= type "Topic")
	     (delete-topic-from-jtm delete :revision revision))
	    ((string= type "PSI")
	     (delete-identifier-from-jtm delete 'd:PersistentIdC
					    #'d:delete-psi :revision revision))
	    ((string= type "ItemIdentity")
	     (delete-identifier-from-jtm delete 'd:ItemIdentifierC
					    #'d:delete-item-identifier
					    :revision revision))
	    ((string= type "SubjectLocator")
	     (delete-identifier-from-jtm delete 'd:SubjectLocatorC
					    #'d:delete-locator :revision revision))
	    ((string= type "Name")
	     (delete-name-from-jtm  delete :revision revision))
	    ((string= type "Variant")
	     (delete-variant-from-jtm delete :revision revision))
	    ((string= type "Occurrence")
	     (delete-occurrence-from-jtm delete :revision revision))
	    ((string= type "Association")
	     (delete-association-from-jtm delete :revision revision))
	    ((string= type "Role")
	     (delete-role-from-jtm delete :revision revision))
	    (t
	     (error "Type \"~a\" is not defined" type))))))


(defun delete-role-from-jtm (jtm-decoded-list
			     &key (revision *TM-REVISION*))
  "Deletes the passed role object and returns t otherwise this
   function returns nil."
  (declare (list jtm-decoded-list) (integer revision))
  (let* ((prefs (jtm::make-prefix-list-from-jtm-list
		 (jtm::get-item :PREFIXES jtm-decoded-list)))
	 (ii
	  (let ((curies (jtm::get-item :ITEM--IDENTIFIERS jtm-decoded-list)))
	    (when curies
	      (jtm::compute-uri-from-jtm-identifier (first curies) prefs))))
	 (type
	  (let ((curie (jtm::get-item :TYPE jtm-decoded-list)))
	    (when curie
	      (jtm::get-item-from-jtm-reference curie :revision revision
						:prefixes prefs))))
	 (reifier
	  (let ((curie (jtm::get-item :REIFIER jtm-decoded-list)))
	    (when curie
	      (jtm::get-item-from-jtm-reference
	       curie :revision revision :prefixes prefs))))
	 (parent
	  (let* ((curies (jtm::get-item :PARENT jtm-decoded-list))
		 (parents (jtm::get-items-from-jtm-references
			   curies :revision revision :prefixes prefs)))
	    (when parents
	      (first parents))))
	 (player-top
	  (let ((curie (jtm::get-item :PLAYER jtm-decoded-list)))
	    (when curie
	      (jtm::get-item-from-jtm-reference curie :revision revision
						:prefixes prefs)))))
    (let ((role-to-delete
	   (cond (ii
		  (identified-construct ii :revision revision))
		 (reifier
		  (reified-construct reifier :revision revision))
		 (parent
		  (let ((found-roles
			 (tools:remove-null
			  (map 'list (lambda(role)
				       (when (d::equivalent-construct
					      role :start-revision revision
					      :player player-top
					      :instance-of type)
					 role))
			       (roles parent :revision revision)))))
		    (when found-roles
		      (first found-roles))))
		 (t
		  (error "when deleting a role, there must be an item-identifier, reifier or parent set!")))))
      (when role-to-delete
	(delete-role (parent role-to-delete :revision revision)
			role-to-delete :revision revision)
	role-to-delete))))
	 



(defun delete-association-from-jtm (jtm-decoded-list &key
				     (revision *TM-REVISION*))
  "Deletes the passed association object and returns t otherwise this
   function returns nil."
  (declare (list jtm-decoded-list) (integer revision))
  (let* ((prefs (jtm::make-prefix-list-from-jtm-list
		 (jtm::get-item :PREFIXES jtm-decoded-list)))
	 (ii
	  (let ((curies (jtm::get-item :ITEM--IDENTIFIERS jtm-decoded-list)))
	    (when curies
	      (jtm::compute-uri-from-jtm-identifier (first curies) prefs))))
	 (scope
	  (let ((curies (jtm::get-item :SCOPE jtm-decoded-list)))
	    (jtm::get-items-from-jtm-references
	     curies :revision revision :prefixes prefs)))
	 (type
	  (let ((curie (jtm::get-item :TYPE jtm-decoded-list)))
	    (when curie
	      (jtm::get-item-from-jtm-reference curie :revision revision
						:prefixes prefs))))
	 (reifier
	  (let ((curie (jtm::get-item :REIFIER jtm-decoded-list)))
	    (when curie
	      (jtm::get-item-from-jtm-reference
	       curie :revision revision :prefixes prefs))))
	 (roles
	  (map 'list (lambda(jtm-role)
		       (jtm::make-plist-of-jtm-role
			jtm-role :revision revision :prefixes prefs))
	       (jtm::get-item :ROLES jtm-decoded-list))))
    (let ((assoc-to-delete
	   (cond (ii
		  (identified-construct ii :revision revision))
		 (reifier
		  (reified-construct reifier :revision revision))
		 (t
		  (let ((found-assocs
			 (tools:remove-null
			  (map 'list (lambda(assoc)
				       (d::equivalent-construct
					assoc :start-revision revision
					:roles roles :instance-of type
					:themes scope))
			       (get-all-associations revision)))))
		    (when found-assocs
		      (first found-assocs)))))))
      (when assoc-to-delete
	(mark-as-deleted assoc-to-delete :revision revision)
	assoc-to-delete))))


(defun delete-variant-from-jtm (jtm-decoded-list
				 &key (revision *TM-REVISION*))
  "Deletes the passed variant from the given name and returns t if the
   operation succeeded."
  (declare (list jtm-decoded-list) (integer revision))
  (let* ((prefs (jtm::make-prefix-list-from-jtm-list
		 (jtm::get-item :PREFIXES jtm-decoded-list)))
	 (ii
	  (let ((curies (jtm::get-item :ITEM--IDENTIFIERS jtm-decoded-list)))
	    (when curies
	      (jtm::compute-uri-from-jtm-identifier (first curies) prefs))))
	 (value (jtm::get-item :VALUE jtm-decoded-list))
	 (datatype (jtm::get-item :DATATYPE jtm-decoded-list))
	 (scope
	  (let ((curies (jtm::get-item :SCOPE jtm-decoded-list)))
	    (jtm::get-items-from-jtm-references
	     curies :revision revision :prefixes prefs)))
	 (parent
	  (let* ((curies (jtm::get-item :PARENT jtm-decoded-list))
		 (parents (jtm::get-items-from-jtm-references
			   curies :revision revision :prefixes prefs)))
	    (when parents
	      (first parents))))
	 (reifier
	  (let ((curie (jtm::get-item :REIFIER jtm-decoded-list)))
	    (when curie
	      (jtm::get-item-from-jtm-reference
	       curie :revision revision :prefixes prefs)))))
    (let ((var-to-delete
	   (cond (ii
		  (identified-construct ii :revision revision))
		 (reifier
		  (reified-construct reifier :revision revision))
		 (parent
		  (let ((found-vars
			 (tools:remove-null
			  (map 'list (lambda(var)
				       (when (d::equivalent-construct
					      var :start-revision revision
					      :charvalue value :themes scope
					      :datatype datatype)
					 var))
			       (variants parent :revision revision)))))
		    (when found-vars
		      (first found-vars))))
		 (t
		  (error "when deleting a variant, there must be an item-identifier, reifier or parent set!")))))
      (when var-to-delete
	(delete-variant (parent var-to-delete :revision revision)
			var-to-delete :revision revision)
	var-to-delete))))


(defun delete-occurrence-from-jtm (jtm-decoded-list
				   &key (revision *TM-REVISION*))
  "Deletes the passed occurrence from the given topic and returns t if the
   operation succeeded."
  (declare (list jtm-decoded-list) (integer revision))
  (let* ((prefs (jtm::make-prefix-list-from-jtm-list
		 (jtm::get-item :PREFIXES jtm-decoded-list)))
	 (ii
	  (let ((curies (jtm::get-item :ITEM--IDENTIFIERS jtm-decoded-list)))
	    (when curies
	      (jtm::compute-uri-from-jtm-identifier (first curies) prefs))))
	 (value (jtm::get-item :VALUE jtm-decoded-list))
	 (datatype
	  (let ((curie (jtm::get-item :DATATYPE jtm-decoded-list)))
	    (cond ((null curie)
		   constants:*xml-string*)
		  ((and (tools:string-starts-with curie "[")
			(tools:string-ends-with curie "]"))
		   (jtm::compute-uri-from-jtm-identifier curie prefs))
		  (t
		   curie))))
	 (type
	  (let ((curie (jtm::get-item :TYPE jtm-decoded-list)))
	    (when curie
	      (jtm::get-item-from-jtm-reference curie :revision revision
						:prefixes prefs))))
	 (scope
	  (let ((curies (jtm::get-item :SCOPE jtm-decoded-list)))
	    (jtm::get-items-from-jtm-references
	     curies :revision revision :prefixes prefs)))
	 (parent
	  (let* ((curies (jtm::get-item :PARENT jtm-decoded-list))
		 (parents (jtm::get-items-from-jtm-references
			   curies :revision revision :prefixes prefs)))
	    (when parents
	      (first parents))))
	 (reifier
	  (let ((curie (jtm::get-item :REIFIER jtm-decoded-list)))
	    (when curie
	      (jtm::get-item-from-jtm-reference
	       curie :revision revision :prefixes prefs)))))
    (let ((occ-to-delete
	   (cond (ii
		  (identified-construct ii :revision revision))
		 (reifier
		  (reified-construct reifier :revision revision))
		 (parent
		  (let ((found-occs
			 (tools:remove-null
			  (map 'list (lambda(occ)
				       (when (d::equivalent-construct
					      occ :start-revision revision
					      :charvalue value :themes scope
					      :instance-of type :datatype datatype)
					 occ))
			       (occurrences parent :revision revision)))))
		    (when found-occs
		      (first found-occs))))
		 (t
		  (error "when deleting an occurrence, there must be an item-identifier, reifier or parent set!")))))
      (when occ-to-delete
	(delete-occurrence (parent occ-to-delete :revision revision)
			   occ-to-delete :revision revision)
	occ-to-delete))))


(defun delete-name-from-jtm (jtm-decoded-list
			      &key (revision *TM-REVISION*))
  (declare (list jtm-decoded-list) (integer revision))
  (let* ((prefs (jtm::make-prefix-list-from-jtm-list
		 (jtm::get-item :PREFIXES jtm-decoded-list)))
	 (ii
	  (let ((curies (jtm::get-item :ITEM--IDENTIFIERS jtm-decoded-list)))
	    (when curies
	      (jtm::compute-uri-from-jtm-identifier (first curies) prefs))))
	 (value (jtm::get-item :VALUE jtm-decoded-list))
	 (type
	  (let ((curie (jtm::get-item :TYPE jtm-decoded-list)))
	    (if curie
		(jtm::get-item-from-jtm-reference curie :revision revision
						  :prefixes prefs)
		(get-item-by-psi constants:*topic-name-psi*
				 :revision revision :error-if-nil t))))
	 (scope
	  (let ((curies (jtm::get-item :SCOPE jtm-decoded-list)))
	    (jtm::get-items-from-jtm-references
	     curies :revision revision :prefixes prefs)))
	 (parent
	  (let* ((curies (jtm::get-item :PARENT jtm-decoded-list))
		 (parents (jtm::get-items-from-jtm-references
			   curies :revision revision :prefixes prefs)))
	    (when parents
	      (first parents))))
	 (reifier
	  (let ((curie (jtm::get-item :REIFIER jtm-decoded-list)))
	    (when curie
	      (jtm::get-item-from-jtm-reference
	       curie :revision revision :prefixes prefs)))))
    (let ((name-to-delete
	   (cond (ii
		  (identified-construct ii :revision revision))
		 (reifier
		  (reified-construct reifier :revision revision))
		 (parent
		  (let ((found-names
			 (tools:remove-null
			  (map 'list (lambda(name)
				       (when (d::equivalent-construct
					      name :start-revision revision
					      :charvalue value :themes scope
					      :instance-of type)
					 name))
			       (names parent :revision revision)))))
		    (when found-names
		      (first found-names))))
		 (t
		  (error "when deleting a name, there must be an item-identifier, reifier or parent set!")))))
      (when name-to-delete
	(delete-name (parent name-to-delete :revision revision)
		     name-to-delete :revision revision)
	name-to-delete))))


(defun delete-identifier-from-json (uri class delete-function
				    &key (revision *TM-REVISION*))
  "Deleted the passed identifier of the construct it is associated with.
   Returns t if there was deleted an item otherweise it returns nil."
  (declare (string uri) (integer revision) (symbol class))
  (let ((id (elephant:get-instance-by-value
	      class 'd:uri uri)))
    (if (and id (typep id class))
	(progn
	  (apply delete-function
		 (list (d:identified-construct id :revision revision)
		       id :revision revision))
	  id)
	nil)))


(defun delete-topic-from-jtm (jtm-decoded-list &key (revision *TM-REVISION*))
  "Searches for a topic corresponding to the given identifiers.
   Returns t if there was deleted an item otherweise it returns nil."
  (declare (list jtm-decoded-list) (integer revision))
  (let* ((prefs
	  (jtm::make-prefix-list-from-jtm-list
	   (jtm::get-item :PREFIXES jtm-decoded-list)))
	 (ids (append
	       (jtm::get-item :SUBJECT--IDENTIFIERS jtm-decoded-list)
	       (jtm::get-item :ITEM--IDENTIFIERS jtm-decoded-list)
	       (jtm::get-item :SUBJECT--LOCATORS jtm-decoded-list)))
	 (uri (if (null ids)
		  (error (make-condition 'exceptions::JTM-error :message (format nil "From merge-topic-from-jtm-list(): the passed topic has to own at least one identifier: ~a" jtm-decoded-list)))
		  (jtm::compute-uri-from-jtm-identifier (first ids) prefs))))
    (let ((top-to-delete (get-item-by-any-id uri :revision revision)))
      (when top-to-delete
	(mark-as-deleted top-to-delete :source-locator uri :revision revision)
	top-to-delete))))


(defun delete-identifier-from-jtm (uri class delete-function
				   &key (revision *TM-REVISION*))
  "Deleted the passed identifier of the construct it is associated with.
   Returns t if there was deleted an item otherweise it returns nil."
  (declare (string uri) (integer revision) (symbol class))
  (let ((id (elephant:get-instance-by-value
	     class 'd:uri uri)))
    (when (and id (typep id class))
      (apply delete-function
	     (list (d:identified-construct id :revision revision)
		   id :revision revision)))))