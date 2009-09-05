;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2009 Marc Kuester, Christoph Ludwig, Lukas Giessmann
;;+
;;+  Isidorus is freely distributable under the LGPL license.
;;+  You can find a detailed description in trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------
(in-package :rdf-importer)

(defun map-to-tm (tm-id start-revision
		  &key (document-id *document-id*))
  (let ((topics-to-map (get-isi-topics tm-id start-revision
				       :document-id document-id)))
    ))


(defun get-isi-topics (tm-id start-revision
		       &key (document-id *document-id*))
  "Returns all topics of the given tm and revision."
  (let ((isi-topic-type (get-item-by-id *tm2rdf-topic-type-uri* 
					:xtm-id document-id
					:revision start-revision))
	(type-instance (get-item-by-psi *type-instance-psi*
					:revision start-revision))
	(instance (get-item-by-psi *instance-psi*
				   :revision start-revision)))
    (when (and isi-topic-type type-instance instance)
      (with-revision start-revision
	(let ((type-associations
	       (remove-if  #'null
			   (map 'list
				#'(lambda(role)
				    (when (eql (instance-of (parent role))
					       type-instance)
				      (parent role)))
				(player-in-roles isi-topic-type)))))
	  (let ((instances
		 (remove-if #'null
			    (map 'list
				 #'(lambda(assoc)
				     (let ((role
					    (find-if #'(lambda(role)
							 (eql (instance-of role)
							      instance))
						     (roles assoc))))
				       (when role
					 (player role))))
				 type-associations))))
	    (let ((instances-of-tm
		   (with-tm (start-revision document-id tm-id)
		     (intersection (topics xml-importer::tm) instances))))
	      (remove-if #'null
			 (map 'list 
			      #'(lambda(x)
				  (find-item-by-revision x start-revision))
			      instances-of-tm)))))))))
  

(defun map-isi-identifiers (top start-revision
			    &key (prop-uri *tm2rdf-itemIdentity-property*))
  (declare (TopicC top))
  (with-revision start-revision
    (let ((identifier-occs
	   (remove-if #'null
		      (map 'list
			   #'(lambda(occurrence)
			       (let ((type (instance-of occurrence)))
				 (let ((type-psi
					(find-if #'(lambda(psi)
						     (string= prop-uri 
							      (uri psi)))
						 (psis type))))
				   (format t "~a~%" type-psi)
				   (when type-psi
				     occurrence))))
			   (occurrences top)))))
      identifier-occs)))