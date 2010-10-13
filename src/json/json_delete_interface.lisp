;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(defpackage :json-delete-interface
  (:use :cl :datamodel :json-importer)
  (:export :mark-as-deleted-from-json))

(in-package :json-delete-interface)


(defun mark-as-deleted-from-json (json-data &key (revision *TM-REVISION*))
  "Marks an object that is specified by the given JSON data as deleted."
  (declare (string json-data) (integer revision))
  (let ((json-list (json:decode-json-from-string json-data)))
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
	     (delete-topic-from-json delete :revision revision))
	    ((string= type "PSI")
	     (delete-identifier-from-json delete 'd:PersistentIdC
					    #'d:delete-psi :revision revision))
	    ((string= type "ItemIdentity")
	     (delete-identifier-from-json delete 'd:ItemIdentifierC
					    #'d:delete-item-identifier
					    :revision revision))
	    ((string= type "SubjectLocator")
	     (delete-identifier-from-json delete 'd:SubjectLocatorC
					    #'d:delete-locator :revision revision))
	    ((string= type "Name")
	     (delete-name-from-json
	      delete (find-parent parent :revision revision) :revision revision))
	    ((string= type "Variant")
	     (let ((parent-top (find-parent parent-of-parent :revision revision)))
	       (delete-variant-from-json
		delete (find-parent parent :parent-of-parent parent-top
				    :revision revision) :revision revision)))
	    ((string= type "Occurrence")
	     (delete-occurrence-from-json
	      delete (find-parent parent :revision revision) :revision revision))
	    ((string= type "Association")
	     (delete-association-from-json delete :revision revision))
	    ((string= type "Role")
	     (delete-role-from-json delete (find-parent parent :revision revision)))
	    (t
	     (error "Type \"~a\" is not defined" type))))))


(defun delete-role-from-json (json-decoded-list parent-assoc
			      &key (revision *TM-REVISION*))
  "Deletes the passed role object and returns t otherwise this
   function returns nil."
  (declare (list json-decoded-list) (integer revision))
  (let ((j-role (make-role-plist json-decoded-list)))
    (when parent-assoc
      (let ((role-to-delete
	     (loop for role in (d:roles parent-assoc :revision revision)
		when (and
		      (eql
		       (d:instance-of role :revision revision)
		       (getf j-role :type))
		      (eql
		       (d:player role :revision revision)
		       (getf j-role :topicRef)))
		return role)))
	(when role-to-delete
	  (d:delete-role parent-assoc role-to-delete :revision revision)
	  t)))))


(defun delete-association-from-json (json-decoded-list &key
				     (revision *TM-REVISION*))
  "Deletes the passed association object and returns t otherwise this
   function returns nil."
  (declare (list json-decoded-list) (integer revision))
  (let ((assoc (find-association json-decoded-list :revision revision)))
    (when assoc
      (d:mark-as-deleted assoc :revision revision :source-locator nil)
      t)))


(defun make-role-plist (json-decoded-list &key (revision *TM-REVISION*))
  "Returns a plist that represents a list of association roles
   of the passed json-decoded-list."
  (declare (list json-decoded-list) (integer revision))
  (let ((type nil)
	(player nil))
    (loop for j-entry in json-decoded-list
       do (let ((st (car j-entry))
		(nd (cdr j-entry)))
	    (cond ((eql st :topic-Ref)
		   (setf player
			 (json-importer::psis-to-topic nd :revision revision)))
		  ((eql st :type)
		   (setf type
			 (json-importer::psis-to-topic nd :revision revision))))))
    (list :type type :topicRef player)))
    

(defun find-association (json-decoded-list &key (revision *TM-REVISION*))
  "Returns an association object."
  (declare (list json-decoded-list) (integer revision))
  (let ((j-roles nil)
	(type nil)
	(scopes nil))
    (loop for j-entry in json-decoded-list
       do (let ((st (car j-entry))
		(nd (cdr j-entry)))
	    (cond ((eql st :roles)
		   (setf j-roles
			 (map 'list #'(lambda(j-role)
					(make-role-plist j-role :revision revision))
			      nd)))
		  ((eql st :type)
		   (setf type (json-importer::psis-to-topic nd :revision revision)))
		  ((eql st :scopes)
		   (setf scopes (json-importer::json-to-scope nd revision))))))
    (loop for assoc in (d:get-all-associations revision)
       when (and
	     (not
	      (set-exclusive-or
	       (d:roles assoc :revision revision)
	       j-roles
	       :test #'(lambda(a-role j-role)
			 (and (eql (d:instance-of a-role :revision revision)
				   (getf j-role :type))
			      (eql (d:player a-role :revision revision)
				   (getf j-role :topicRef))))))
	     (eql type (d:instance-of assoc :revision revision))
	     (not (set-exclusive-or scopes (d:themes assoc :revision revision))))
       return assoc)))


(defun find-parent (parent &key (parent-of-parent nil)
		    (revision *TM-REVISION*))
  "Returns the construct (Topic|Name|Association) corresponding to the
   passed parameters."
  (declare (list parent) (integer revision)
	   (type (or TopicC null) parent-of-parent))
  (let ((value nil)
	(scopes nil) 
	(type nil)
	(j-roles nil))
    (loop for j-entry in parent
       do (let ((st (car j-entry))
		(nd (cdr j-entry)))
	    (cond ((eql st :value)
		   (setf value nd))
		  ((eql st :scopes)
		   (setf scopes (json-importer::json-to-scope nd revision)))
		  ((eql st :type)
		   (setf type (json-importer::psis-to-topic nd :revision revision)))
		  ((eql st :roles)
		   (setf j-roles nd)))))
    (cond (parent-of-parent
	   (loop for name in (d:names parent-of-parent :revision revision)
	      when (and (string= value (d:charvalue name))
			(eql type (d:instance-of name :revision revision))
			(not (set-exclusive-or scopes
					       (d:themes name :revision revision))))
	      return name))
	  (j-roles ;must be an association
	   (find-association parent :revision revision))
	  (t ;must be a topic
	   (find-topic-from-json-identifiers
	    parent :revision revision)))))


(defun delete-variant-from-json (json-decoded-list parent-name
				 &key (revision *TM-REVISION*))
  "Deletes the passed variant from the given name and returns t if the
   operation succeeded."
  (declare (list json-decoded-list) (integer revision)
	   (type (or NameC null)))
  (when parent-name
    (let ((varvalue nil)
	  (vardatatype constants::*xml-uri*)
	  (scopes nil))
      (loop for j-entry in json-decoded-list
	 do (let ((st (car j-entry))
		  (nd (cdr j-entry)))
	      (cond ((eql st :resource-ref)
		     (setf varvalue nd))
		    ((eql st :resource-data)
		     (loop for j-dt in nd
			do (let ((dt-st (car j-dt))
				 (dt-nd (cdr j-dt)))
			     (cond ((eql dt-st :datatype)
				    (setf vardatatype dt-nd))
				   ((eql dt-st :value)
				    (setf varvalue dt-nd))))))
		    ((eql st :scopes)
		     (setf scopes (json-importer::json-to-scope nd revision))))))
      (let ((var-to-delete
	     (loop for var in (d:variants parent-name :revision revision)
		when (and (string= varvalue (d:charvalue var))
			  (string= vardatatype (d:datatype var))
			  (not (set-exclusive-or
				scopes (d:themes var :revision revision))))
		return var)))	(when var-to-delete
	  (delete-variant parent-name var-to-delete :revision revision)
	  t)))))


(defun delete-occurrence-from-json (json-decoded-list parent-top
				    &key (revision *TM-REVISION*))
  "Deletes the passed occurrence from the given topic and returns t if the
   operation succeeded."
  (declare (list json-decoded-list) (integer revision))
  (when parent-top
    (let ((occvalue nil)
	  (occdatatype constants::*xml-uri*)
	  (scopes nil)
	  (type nil))
      (loop for j-entry in json-decoded-list
	 do (let ((st (car j-entry))
		  (nd (cdr j-entry)))
	      (cond ((eql st :resource-ref)
		     (setf occvalue nd))
		    ((eql st :resource-data)
		     (loop for j-dt in nd
			do (let ((dt-st (car j-dt))
				 (dt-nd (cdr j-dt)))
			     (cond ((eql dt-st :datatype)
				    (setf occdatatype dt-nd))
				   ((eql dt-st :value)
				    (setf occvalue dt-nd))))))
		    ((eql st :scopes)
		     (setf scopes (json-importer::json-to-scope nd revision)))
		    ((eql st :type)
		     (setf type (json-importer::psis-to-topic
				 nd :revision revision))))))
      (let ((occ-to-delete
	     (loop for occ in (d:occurrences parent-top :revision revision)
		when (and (string= occvalue (d:charvalue occ))
			  (string= occdatatype (d:datatype occ))
			  (eql type (d:instance-of occ :revision revision))
			  (not (set-exclusive-or
				scopes (d:themes occ :revision revision))))
		return occ)))
	(when occ-to-delete
	  (delete-occurrence parent-top occ-to-delete :revision revision)
	  t)))))


(defun delete-name-from-json (json-decoded-list parent-top
			      &key (revision *TM-REVISION*))
  (declare (list json-decoded-list) (integer revision))
  (when parent-top
    (let ((namevalue nil)
	  (scopes nil)
	  (type nil))
      (loop for j-entry in json-decoded-list
	 do (let ((st (car j-entry))
		  (nd (cdr j-entry)))
	      (cond ((eql st :value)
		     (setf namevalue nd))
		    ((eql st :scopes)
		     (setf scopes (json-importer::json-to-scope nd revision)))
		    ((eql st :type)
		     (setf type (json-importer::psis-to-topic
				 nd :revision revision))))))
      (let ((name-to-delete
	     (loop for name in (names parent-top :revision revision)
		when (and (string= namevalue (d:charvalue name))
			  (eql type (d:instance-of name :revision revision))
			  (not (set-exclusive-or
				scopes (d:themes name :revision revision))))
		return name)))
	(when name-to-delete
	  (delete-name parent-top name-to-delete :revision revision)
	  t)))))


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
	  t)
	nil)))


(defun delete-topic-from-json (json-decoded-list &key (revision *TM-REVISION*))
  "Searches for a topic corresponding to the given identifiers.
   Returns t if there was deleted an item otherweise it returns nil."
  (declare (list json-decoded-list) (integer revision))
  (let ((top-to-delete (find-topic-from-json-identifiers
			json-decoded-list :revision revision)))
    (when top-to-delete
      (mark-as-deleted top-to-delete :source-locator nil :revision revision)
      t)))


(defun get-ids-from-json (json-decoded-list)
  "Returns all id uri formatted as plist generated from the json-list."
  (let ((iis nil)
	(psis nil)
	(sls nil))
    (loop for json-entry in json-decoded-list
       do (let ((st (car json-entry))
		(nd (cdr json-entry)))
	    (cond ((eql st :item-identities)
		   (setf iis nd))
		  ((eql st :subject-locators)
		   (setf sls nd))
		  ((eql st :subject-identifiers)
		   (setf psis nd)))))
    (list :subjectIdentifiers psis
	  :itemIdentities iis
	  :subjectLocators sls)))


(defun find-topic-from-json-identifiers (json-decoded-list
					 &key (revision *TM-REVISION*))
  "Returns a topic corresponding to the passed identifiers."
  (declare (list json-decoded-list) (integer revision))
  (let ((ids (get-ids-from-json json-decoded-list)))
    (let ((identifier
	   (if (getf ids :itemIdentities)
	       (elephant:get-instance-by-value
		'd:ItemIdentifierC 'd:uri (first (getf ids :itemIdentities)))
	       (if (getf ids :subjectIdentifiers)
		   (elephant:get-instance-by-value
		    'd:PersistentIdC 'd:uri (first (getf ids :subjectIdentifiers)))
		   (when (getf ids :subjectLocators)
		     (elephant:get-instance-by-value
		      'd:SubjectLocatorC 'd:uri
		      (first (getf ids :subjectLocators))))))))
    (when identifier
      (d:identified-construct identifier :revision revision)))))