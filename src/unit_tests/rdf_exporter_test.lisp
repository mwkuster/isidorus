;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :rdf-exporter-test
  (:use 
   :common-lisp
   :xml-importer
   :datamodel
   :it.bese.FiveAM
   :fixtures
   :base-tools)
  (:import-from :constants
                *rdf-ns*
		*rdfs-ns*
		*rdf2tm-ns*
		*tm2rdf-ns*
		*xml-ns*
		*xml-string*
		*xml-uri*)
  (:import-from :xml-tools
                xpath-child-elems-by-qname
		xpath-single-child-elem-by-qname
                xpath-select-location-path
		get-ns-attribute)
  (:export :run-rdf-exporter-tests
	   :test-resources
	   :test-goethe
	   :test-erlkoenig
	   :test-prometheus
	   :test-zauberlehrling
	   :test-frankfurt
	   :test-weimar
	   :test-berlin
	   :test-region
	   :test-city-and-metropolis
	   :test-germany
	   :test-german
	   :test-born-event
	   :test-died-event
	   :test-dateRange-zauberlehrling
	   :test-dateRange-erlkoenig
	   :test-dateRange-prometheus
	   :test-schiller
	   :test-single-nodes
	   :test-collection
	   :test-association))

(declaim (optimize (debug 3) (speed 0) (safety 3) (space 0) (compilation-speed 0)))

(in-package :rdf-exporter-test)


(def-suite rdf-exporter-test
     :description "tests  various key functions of the exporter")

(in-suite rdf-exporter-test)


(defvar *sw-arc* "http://some.where/relationship/")
(defvar *xml-ulong* "http://www.w3.org/2001/XMLSchema#unsignedLong")
(defvar *xml-date* "http://www.w3.org/2001/XMLSchema#date")


(defun get-dom-root ()
  "Returns the document's root node."
  (let ((dom (cxml:parse-file "./__out__.rdf" (cxml-dom:make-dom-builder))))
    (when dom
      (let ((child-nodes (dom:child-nodes dom)))
	(when (> (length child-nodes) 0)
	  (elt child-nodes 0))))))


(defun identifier-p (owner-elem value &key (what "itemIdentity"))
  "Returns t if the owner element owns a property correponding to the
   attribute what and the value."
  (literal-p owner-elem *tm2rdf-ns* what value :datatype *xml-uri*))


(defun role-p (owner-elem roletype-uri item-identifiers 
	       &key (player-uri nil) (player-id nil))
  "Returns t if the owner-element has a node that corresponds to a
   role with the given parameters."
  (loop for item across (dom:child-nodes owner-elem)
     when (let* ((node-ns (dom:namespace-uri item))
		 (node-name (rdf-importer::get-node-name item))
		 (content (rdf-importer::child-nodes-or-text item :trim t))
		 (descr (when (and (not (stringp content))
				   (= (length content) 1))
			  (elt content 0))))
	    (and descr
		 (string= (dom:namespace-uri descr) *rdf-ns*)
		 (string= (rdf-importer::get-node-name descr) "Description")
		 (= (length (dom:child-nodes descr)) 
		    (+ 3 (length item-identifiers)))
		 (string= node-ns *tm2rdf-ns*)
		 (string= node-name "role")
		 (type-p descr (concat *tm2rdf-ns* "types/Role"))
		 (if player-uri
		     (property-p descr *tm2rdf-ns* "player"
				 :resource player-uri)
		     (property-p descr *tm2rdf-ns* "player"
				 :nodeID player-id))
		 (property-p descr *tm2rdf-ns* "roletype"
			     :resource roletype-uri)
		 (= (length item-identifiers)
		    (length (loop for ii in item-identifiers
			       when (identifier-p descr ii)
			       collect ii)))))
     return t))


(defun get-resources-by-uri (uri)
  "Returns a list of resource elements that owns the attribute
   about with the value of uri."
  (let ((root (get-dom-root)))
    (let ((resources (xpath-child-elems-by-qname root *rdf-ns* "Description")))
      (loop for item across resources
	 when (string= (get-ns-attribute item "about") uri)
	 collect item))))


(defun get-resources-by-id (id)
  "Returns a list of resource elements that owns the attribute
   nodeID with the value of id."
  (let ((root (get-dom-root)))
    (let ((resources (xpath-child-elems-by-qname root *rdf-ns* "Description")))
      (loop for item across resources
	 when (string= (get-ns-attribute item "nodeID") id)
	 collect item))))


(defun type-p (owner-elem type-uri)
  "Returns t if the given uri is contained in a property
   within the owner-elem."
  (loop for item across (dom:child-nodes owner-elem)
     when (let ((node-ns (dom:namespace-uri item))
		(node-name (rdf-importer::get-node-name item))
		(resource (rdf-importer::get-ns-attribute
			   item "resource")))
	    (and (string= node-ns *rdf-ns*)
		 (string= node-name "type")
		 (string= resource type-uri)))
     return t))


(defun literal-p (owner-elem arc-uri arc-name literal-value
		  &key (datatype *xml-string*)
		  (xml-lang nil))
  "Returns t if the owner-elem contains an arc with the uri
   arc-uri, the arc-name and the literal content literal-value."
  (loop for item across (dom:child-nodes owner-elem)
     when (let ((node-ns (dom:namespace-uri item))
		(node-name (rdf-importer::get-node-name item))
		(value (rdf-importer::child-nodes-or-text item :trim nil))
		(fn-datatype (rdf-importer::get-ns-attribute item "datatype"))
		(fn-xml-lang (rdf-importer::get-ns-attribute
			      item "lang" :ns-uri *xml-ns*)))
	    (and (string= node-ns arc-uri)
		 (string= node-name arc-name)
		 (and (stringp literal-value)
		      (string= value literal-value))
		 (string= datatype (if fn-datatype
				       fn-datatype
				       ""))
		 (or (not (or xml-lang fn-xml-lang))
		     (and (and xml-lang fn-xml-lang)
			  (string= xml-lang fn-xml-lang)))))
     return t))


(defun property-p (owner-elem arc-uri arc-name
		   &key (resource "") (nodeID ""))
  "Returns t if the owner element owns a property with the
   given characteristics."
  (if (and (string= resource "") (string= nodeID ""))
      nil
      (loop for item across (dom:child-nodes owner-elem)
	 when (let ((node-ns (dom:namespace-uri item))
		    (node-name (rdf-importer::get-node-name item))
		    (fn-resource (unless (dom:text-node-p  item)
				   (rdf-importer::get-ns-attribute item
								   "resource")))
		    (fn-nodeID (rdf-importer::get-ns-attribute item "nodeID")))
		(and (string= node-ns arc-uri)
		     (string= node-name arc-name)
		     (or (and fn-resource
			      (string= fn-resource resource))
			 (and fn-nodeID
			      (string= fn-nodeID nodeID)))))
	 return t)))


(defun variant-p (owner-elem variant-scopes item-identifiers variant-value
		  &key (datatype *xml-string*))
  "Returns t if the owner contains a variant element with the passed
   characteristics."
    (loop for item across (dom:child-nodes owner-elem)
       when (let* ((node-ns (dom:namespace-uri item))
		   (node-name (rdf-importer::get-node-name item))
		   (content (rdf-importer::child-nodes-or-text item :trim t))
		   (descr (when (and (not (stringp content))
				     (= (length content) 1))
			    (elt content 0))))
	      (and descr
		   (string= (dom:namespace-uri descr) *rdf-ns*)
		   (string= (rdf-importer::get-node-name descr) "Description")
		   (rdf-importer::get-ns-attribute descr "nodeID")
		   (= (+ (length variant-scopes)
			 (length item-identifiers)
			 2)
		      (length (dom:child-nodes owner-elem)))
		   (string= node-ns *tm2rdf-ns*)
		   (string= node-name "variant")
		   (literal-p descr *tm2rdf-ns* "value" variant-value
			      :datatype datatype)
		   (= (length variant-scopes)
		      (length (loop for scope in variant-scopes
				 when (property-p descr *tm2rdf-ns* "scope"
						  :resource scope)
				 collect scope)))
		   (= (length item-identifiers)
		      (length (loop for ii in item-identifiers
				 when (identifier-p descr ii)
				 collect ii)))
		   (type-p descr (concat *tm2rdf-ns* "types/Variant"))))
       return t))


(defun name-p (owner-elem name-type name-scopes item-identifiers name-value
	       &key (variants nil))
  "Returns t if the parent node owns a name with the given characterics."
  (loop for item across (dom:child-nodes owner-elem)
     when (let* ((node-ns (dom:namespace-uri item))
		 (node-name (rdf-importer::get-node-name item))
		 (content (rdf-importer::child-nodes-or-text item :trim t))
		 (descr (when (and (not (stringp content))
				   (= (length content) 1))
			  (elt content 0))))
	    (and descr
		 (string= (dom:namespace-uri descr) *rdf-ns*)
		 (string= (rdf-importer::get-node-name descr) "Description")
		 (rdf-importer::get-ns-attribute descr "nodeID")
		 (= (length (dom:child-nodes descr))
		    (+ 3 (length name-scopes)
		       (length item-identifiers)
		       (length variants)))
		 (string= node-ns *tm2rdf-ns*)
		 (string= node-name "name")
		 (type-p descr (concat *tm2rdf-ns* "types/Name"))
		 (property-p descr *tm2rdf-ns* "nametype" :resource name-type)
		 (= (length name-scopes)
		    (length (loop for scope in name-scopes
			       when (property-p descr *tm2rdf-ns* "scope"
						:resource scope)
			       collect scope)))
		 (= (length item-identifiers)
		    (length (loop for ii in item-identifiers
			       when (identifier-p descr ii)
			       collect ii)))
		 (= (length variants)
		    (length (loop for variant in variants
			       when (variant-p
				     descr (getf variant :scopes)
				     (getf variant :item-identifiers)
				     (getf variant :value)
				     :datatype (getf variant :datatype))
			       collect variant)))
		 (literal-p descr *tm2rdf-ns* "value" name-value)))
     return t))


(defun occurrence-p (owner-elem occurrence-type occurrence-scopes
		     item-identifiers occurrence-value 
		     &key (datatype *xml-string*))
  "Returns t if the parent node owns an occurrence with the given characterics."
  (loop for item across (dom:child-nodes owner-elem)
     when (let* ((node-ns (dom:namespace-uri item))
		 (node-name (rdf-importer::get-node-name item))
		 (content (rdf-importer::child-nodes-or-text item :trim t))
		 (descr (when (and (not (stringp content))
				   (= (length content) 1))
			  (elt content 0))))
	    (and descr
		 (string= (dom:namespace-uri descr) *rdf-ns*)
		 (string= (rdf-importer::get-node-name descr) "Description")
		 (= (length (dom:child-nodes descr))
		    (+ 3 (length occurrence-scopes)
		       (length item-identifiers)))
		 (string= node-ns *tm2rdf-ns*)
		 (string= node-name "occurrence")
		 (type-p descr (concat *tm2rdf-ns* "types/Occurrence"))
		 (property-p descr *tm2rdf-ns* "occurrencetype"
			     :resource occurrence-type)
		 (= (length occurrence-scopes)
		    (length (loop for scope in occurrence-scopes
			       when (property-p descr *tm2rdf-ns* "scope"
						:resource scope)
			       collect scope)))
		 (= (length item-identifiers)
		    (length (loop for ii in item-identifiers
			       when (identifier-p descr ii)
			       collect ii)))
		 (literal-p descr *tm2rdf-ns* "value" occurrence-value
			    :datatype datatype)))
     return t))

	    
(test test-resources
  "Tests the general amount of resources."
  (with-fixture rdf-exporter-test-db ()
    (let ((root (get-dom-root)))
      (is-true root)
      (let ((resources (xpath-child-elems-by-qname root *rdf-ns* "Description")))
	(is (= (length resources) 29))
	(is (= (length (loop for item across resources
			  when (get-ns-attribute item "about")
			  collect item))
	       19))
	(is (= (length (loop for item across resources
			  when (get-ns-attribute item "nodeID")
			  collect item))
	       10))))))


(test test-goethe
  "Tests the resource goethe."
  (with-fixture rdf-exporter-test-db ()
    (let ((goethes (get-resources-by-uri "http://some.where/author/Goethe")))
      (is (= (length goethes) 1))
      (let ((me (find-if #'(lambda(x)
			     (= (length (dom:child-nodes x)) 7))
			 goethes)))
	(is-true me)
	(is (type-p me "http://isidorus/tm2rdf_mapping/types/Topic"))
	(is (type-p me "http://some.where/types/Author"))
	(is (literal-p me *sw-arc* "lastName"
		       "von Goethe"))
	(is (name-p me "http://some.where/relationship/firstName" nil
		    (list "http://some.where/name_ii_1") "Johann Wolfgang"))
	(let ((born-id (concat
			"id_"
			(write-to-string
			 (elephant::oid
			  (d:parent
			   (elephant:get-instance-by-value
			    'd:OccurrenceC 'd:charvalue "28.08.1749"))))))
	      (died-id (concat
			"id_"
			(write-to-string
			 (elephant::oid
			  (d:parent
			   (elephant:get-instance-by-value
			    'd:OccurrenceC 'd:charvalue "22.03.1832")))))))
	  (is-true (property-p me *sw-arc* "born" :nodeID born-id))
	  (is-true (property-p me *sw-arc* "died" :nodeID died-id)))
	(is-true (loop for item across (dom:child-nodes me)
		    when (let ((node-ns (dom:namespace-uri item))
			       (node-name (rdf-importer::get-node-name item))
			       (nodeID (rdf-importer::get-ns-attribute
					item "nodeID")))
			   (and (string= node-ns *sw-arc*)
				(string= node-name "wrote")
				nodeID))
		    return t))))))


(test test-erlkoenig
  "Tests the resource erlkoenig."
  (with-fixture rdf-exporter-test-db ()
    (let ((erlkoenigs (get-resources-by-uri
		       "http://some.where/ballad/Der_Erlkoenig")))
      (is (= (length erlkoenigs) 1))
      (let ((me (find-if #'(lambda(x)
			     (= (length (dom:child-nodes x)) 5))
			 erlkoenigs)))
	(is-true me)
	(is-true (type-p me "http://some.where/types/Ballad"))
	(is-true (type-p me (concat *tm2rdf-ns* "types/Topic")))
	(is-true (literal-p me *sw-arc* "content"
			    "Wer reitet so spät durch Nacht und Wind? ..."
			    :xml-lang "de"))
	(is-true (occurrence-p me "http://some.where/relationship/title"
			       (list "http://some.where/scope/en") nil
			       "Der Erlkönig"))
	(let ((dateRange-id
	       (concat
		"id_"
		(write-to-string
		 (elephant::oid
		  (d:parent
		   (elephant:get-instance-by-value
		    'd:OccurrenceC 'd:charvalue "31.12.1782")))))))
	  (is-true (property-p me *sw-arc* "dateRange"
			       :nodeID dateRange-id)))))))


(test test-prometheus
  "Tests the resoruce prometheus."
  (with-fixture rdf-exporter-test-db ()
    (let ((prometheus (get-resources-by-uri
		       "http://some.where/poem/Prometheus")))
      (is (= (length prometheus) 1))
      (let ((me (find-if #'(lambda(x)
			     (= (length (dom:child-nodes x)) 4))
			 prometheus)))
	(is-true me)
	(is-true (type-p me "http://some.where/types/Poem"))
	(is-true (literal-p me *sw-arc* "title"
			    "Prometheus" :xml-lang "de"))
	(is-true (literal-p me *sw-arc* "content"
			    "Bedecke deinen Himmel, Zeus, ..."
			    :xml-lang "de"))
	(let ((dateRange-id
	       (concat
		"id_"
		(write-to-string
		 (elephant::oid
		  (d:parent
		   (elephant:get-instance-by-value
		    'd:OccurrenceC 'd:charvalue "01.01.1772")))))))
	  (is-true (property-p me *sw-arc* "dateRange"
			       :nodeID dateRange-id)))))))


(test test-zauberlehrling
  "Tests the resource zauberlehrling."
  (with-fixture rdf-exporter-test-db ()
    (let ((zauberlehrlings (get-resources-by-uri
			   "http://some.where/poem/Der_Zauberlehrling")))
      (is (= (length zauberlehrlings) ))
      (let ((me (find-if #'(lambda(x)
			     (= (length (dom:child-nodes x)) 10))
			 zauberlehrlings)))
	(is-true me)
	(is-true (type-p me "http://some.where/types/Poem"))
	(is-true (type-p me (concat *tm2rdf-ns* "types/Topic")))
	(is-true (identifier-p me "http://some.where/poem/Zauberlehrling"
			       :what "subjectIdentifier"))
	(is-true (identifier-p
		  me "http://some.where/poem/Zauberlehrling_itemIdentity_1"))
	(is-true (identifier-p
		  me  "http://some.where/poem/Zauberlehrling_itemIdentity_2"))
	(is-true (identifier-p me "http://some.where/resource_1"
			       :what "subjectLocator"))
	(is-true (identifier-p me "http://some.where/resource_2"
			       :what "subjectLocator"))
	(is-true (literal-p me "http://some.where/relationship/" "content"
			    "Hat der alte Hexenmeister ..."))
	(is-true (occurrence-p me "http://some.where/relationship/title"
			       (list "http://some.where/scope/en"
				     "http://isidorus/rdf2tm_mapping/scope/de")
			       (list "http://some.where/occurrence_ii_1"
				     "http://some.where/occurrence_ii_2")
			       "Der Zauberlehrling"))
	(let ((dateRange-id
	       (concat
		"id_"
		(write-to-string
		 (elephant::oid
		  (d:parent
		   (elephant:get-instance-by-value
		    'd:OccurrenceC 'd:charvalue "01.01.1797")))))))
	  (is-true (property-p me *sw-arc* "dateRange"
			       :nodeID dateRange-id)))))))


(test test-frankfurt
  "Tests the resoruce frankfurt."
  (with-fixture rdf-exporter-test-db ()
    (let ((frankfurts (get-resources-by-uri
			   "http://some.where/metropolis/FrankfurtMain")))
      (is (= (length frankfurts) 1))
      (let ((me (find-if #'(lambda(x)
			     (= (length (dom:child-nodes x)) 4))
			 frankfurts)))
	(is-true me)
	(is-true (type-p me "http://some.where/types/Metropolis"))
	(is-true (literal-p me *sw-arc* "fullName" "Frankfurt am Main"))
	(is-true (literal-p me *sw-arc* "population" "659000" 
			    :datatype *xml-ulong*))
	(is-true (property-p me *sw-arc* "locatedIn"
			     :resource "http://some.where/country/Germany"))))))

(test test-weimar
  "Tests the resoruce weimar."
  (with-fixture rdf-exporter-test-db ()
    (let ((weimars (get-resources-by-uri
			   "http://some.where/city/Weimar")))
      (is (= (length weimars) 1))
      (let ((me (find-if #'(lambda(x)
			     (= (length (dom:child-nodes x)) 4))
			 weimars)))
	(is-true me)
	(is-true (type-p me "http://some.where/types/City"))
	(is-true (literal-p me *sw-arc* "fullName" "Weimar"))
	(is-true (literal-p me *sw-arc* "population" "64720" 
			    :datatype *xml-ulong*))
	(is-true (property-p me *sw-arc* "locatedIn"
			     :resource "http://some.where/country/Germany"))))))


(test test-berlin
  "Tests the resource berlin."
  (with-fixture rdf-exporter-test-db ()
    (let ((berlins (get-resources-by-uri
		    "http://some.where/metropolis/Berlin")))
      (is (= (length berlins) 1))
      (let ((me (find-if #'(lambda(x)
			     (= (length (dom:child-nodes x)) 4))
			 berlins)))
	(is-true me)
	(is-true (type-p me "http://some.where/types/Metropolis"))
	(is-true (literal-p me *sw-arc* "fullName" "Berlin"))
	(is-true (literal-p me *sw-arc* "population" "3431473" 
			    :datatype *xml-ulong*))
	(is-true (property-p me *sw-arc* "locatedIn"
			     :resource "http://some.where/country/Germany"))))))


(test test-region 
  "Tests the resource region."
  (with-fixture rdf-exporter-test-db ()
    (let ((regions (get-resources-by-uri
		    "http://some.where/types/Region"))
	  (citys (get-resources-by-uri
		  "http://some.where/types/City"))
	  (metropolis (get-resources-by-uri
		       "http://some.where/types/Metropolis")))
      (is (= (length regions) 1))
      (is (= (length (dom:child-nodes (elt regions 0))) 0))
      (is (= (length citys) 1))
      (is (= (length (dom:child-nodes (elt citys 0))) 1))
      (is-true (property-p (elt citys 0) *rdfs-ns* "subClassOf"
			   :resource "http://some.where/types/Region"))
      (is (= (length metropolis) 1))
      (is (= (length (dom:child-nodes (elt metropolis 0))) 1))
      (is-true (property-p (elt metropolis 0) *rdfs-ns* "subClassOf"
			   :resource "http://some.where/types/Region")))))


(test test-city-and-metropolis
  "Tests the resource city and metropolis."
  (with-fixture rdf-exporter-test-db ()
      (let ((citys (get-resources-by-uri
		  "http://some.where/types/City")))
      (is (= (length citys) 1))
      (is (= (length (dom:child-nodes (elt citys 0))) 1))
      (is-true (property-p (elt citys 0) *rdfs-ns* "subClassOf"
			   :resource "http://some.where/types/Region")))
    (let ((metropolis (get-resources-by-uri
		       "http://some.where/types/Metropolis")))
      (is (= (length metropolis) 1))
      (is (= (length (dom:child-nodes (elt metropolis 0))) 1))
      (is-true (property-p (elt metropolis 0) *rdfs-ns* "subClassOf"
			   :resource "http://some.where/types/Region")))))


(test test-germany
  "Tests the resource germany."
    (with-fixture rdf-exporter-test-db ()
      (let ((germanys (get-resources-by-uri
		       "http://some.where/country/Germany")))
      (is (= (length germanys) 1))
      (let ((me (find-if #'(lambda(x)
			     (= (length (dom:child-nodes x)) 5))
			 germanys)))
	(is-true me)
	(is-true (type-p me "http://some.where/types/Country"))
	(is-true (literal-p me *sw-arc* "nativeName" "Deutschland"
			    :xml-lang "de"))
	(is-true (literal-p me *sw-arc* "population" "82099232"
			    :datatype *xml-ulong*))
	(is-true (property-p me *sw-arc* "capital"
			     :resource "http://some.where/metropolis/Berlin"))
	(is-true (property-p me *sw-arc* "officialese"
			     :resource "http://some.where/language/German"))))))


(test test-german
  "Tests the resource german."
    (with-fixture rdf-exporter-test-db ()
      (let ((germans (get-resources-by-uri
		       "http://some.where/language/German")))
      (is (= (length germans) 1))
      (is-true (type-p (elt germans 0) "http://some.where/types/Language")))))


(test test-born-event
  "Tests the blank node of the born-event."
  (with-fixture rdf-exporter-test-db ()
    (let ((born-id (concat
		    "id_"
		    (write-to-string
		     (elephant::oid
		      (d:parent
		       (elephant:get-instance-by-value 'd:OccurrenceC
						       'd:charvalue
						       "28.08.1749")))))))
      (is-true born-id)
      (let ((born-events (get-resources-by-id born-id)))
	(is (= (length born-events) 1))
	(let ((me (find-if #'(lambda(x)
			       (= (length (dom:child-nodes x)) 3))
			   born-events)))
	  (is-true me)
	  (is-true (literal-p me *sw-arc* "date" "28.08.1749"
			      :datatype *xml-date*))
	  (is-true (type-p me "http://some.where/types/Event"))
	  (is-true 
	   (property-p me *sw-arc* "place"
		       :resource
		       "http://some.where/metropolis/FrankfurtMain")))))))


(test test-died-event
  "Tests the blank node of the born-event."
  (with-fixture rdf-exporter-test-db ()
    (let ((born-id (concat
		    "id_"
		    (write-to-string
		     (elephant::oid
		      (d:parent
		       (elephant:get-instance-by-value 'd:OccurrenceC
						       'd:charvalue
						       "22.03.1832")))))))
      (is-true born-id)
      (let ((born-events (get-resources-by-id born-id)))
	(is (= (length born-events) 1))
	(let ((me (find-if #'(lambda(x)
			       (= (length (dom:child-nodes x)) 3))
			   born-events)))
	  (is-true me)
	  (is-true (literal-p me *sw-arc* "date" "22.03.1832"
			      :datatype *xml-date*))
	  (is-true (type-p me "http://some.where/types/Event"))
	  (is-true 
	   (property-p me *sw-arc* "place"
		       :resource
		       "http://some.where/city/Weimar")))))))


(test test-dateRange-zauberlehrling
  "Tests the node of zauberlehrling's dateRange."
  (with-fixture rdf-exporter-test-db ()
    (let ((dr-id (concat
		  "id_"
		  (write-to-string
		   (elephant::oid
		    (d:parent
		     (elephant:get-instance-by-value 'd:OccurrenceC
						     'd:charvalue
						     "01.01.1797")))))))
      (is-true dr-id)
      (let ((drs (get-resources-by-id dr-id)))
	(is (= (length drs) 1))
	(let ((me (elt drs 0)))
	  (is-true (literal-p me *sw-arc* "start" "01.01.1797"
			      :datatype *xml-date*))
	  (is-true (literal-p me *sw-arc* "end" "31.12.1797"
			      :datatype *xml-date*)))))))


(test test-dateRange-erlkoenig
  "Tests the node of erlkoenig's dateRange."
  (with-fixture rdf-exporter-test-db ()
    (let ((dr-id (concat
		  "id_"
		  (write-to-string
		   (elephant::oid
		    (d:parent
		     (elephant:get-instance-by-value 'd:OccurrenceC
						     'd:charvalue
						     "01.01.1782")))))))
      (is-true dr-id)
      (let ((drs (get-resources-by-id dr-id)))
	(is (= (length drs) 1))
	(let ((me (elt drs 0)))
	  (is-true (literal-p me *sw-arc* "start" "01.01.1782"
			      :datatype *xml-date*))
	  (is-true (literal-p me *sw-arc* "end" "31.12.1782"
			      :datatype *xml-date*)))))))


(test test-dateRange-prometheus
  "Tests the node of prometheus' dateRange."
  (with-fixture rdf-exporter-test-db ()
    (let ((dr-id (concat
		  "id_"
		  (write-to-string
		   (elephant::oid
		    (d:parent
		     (elephant:get-instance-by-value 'd:OccurrenceC
						     'd:charvalue
						     "01.01.1772")))))))
      (is-true dr-id)
      (let ((drs (get-resources-by-id dr-id)))
	(is (= (length drs) 1))
	(let ((me (elt drs 0)))
	  (is-true (literal-p me *sw-arc* "start" "01.01.1772"
			      :datatype *xml-date*))
	  (is-true (literal-p me *sw-arc* "end" "31.12.1774"
			      :datatype *xml-date*)))))))


(test test-schiller
  "Tests the node of schiller."
  (with-fixture rdf-exporter-test-db ()
    (let ((schiller-id (concat
			"id_"
			(write-to-string
			 (elephant::oid
			  (d:parent
			   (elephant:get-instance-by-value
			    'd:OccurrenceC 'd:charvalue
			    "http://de.wikipedia.org/wiki/Schiller")))))))
      (is-true schiller-id)
      (is (= (length (get-resources-by-id schiller-id)) 1))
      (let ((me (elt (get-resources-by-id schiller-id) 0)))
	(is-true (type-p me "http://some.where/types/Author"))
	(is-true (type-p me (concat *tm2rdf-ns* "types/Topic")))
	(is-true (literal-p me *sw-arc* "authorInfo"
			    "http://de.wikipedia.org/wiki/Schiller"
			    :datatype *xml-uri*))
	(is-true 
	 (name-p me "http://some.where/relationship/firstName"
		 nil nil "Johann Christoph Friedrich"
		 :variants
		 (list
		  (list 
		   :item-identifiers
		   (list "http://some.where/variant_ii_1")
		   :scopes 
		   (list "http://www.topicmaps.org/xtm/1.0/core.xtm#display")
		   :value "Friedrich"
		   :datatype *xml-string*))))
	(is-true 
	 (name-p me "http://some.where/relationship/lastName"
		 nil nil "von Schiller"))))))


(test test-single-nodes
  "Tests all nodes that are not part of a statement."
  (with-fixture rdf-exporter-test-db ()
    (let ((authors (get-resources-by-uri "http://some.where/types/Author"))
	  (events (get-resources-by-uri "http://some.where/types/Event"))
	  (country (get-resources-by-uri "http://some.where/types/Country"))
	  (poem (get-resources-by-uri "http://some.where/types/Poem"))
	  (ballad (get-resources-by-uri "http://some.where/types/Ballad"))
	  (language (get-resources-by-uri "http://some.where/types/Language"))
	  (rdf-nil (get-resources-by-uri (concat *rdf-ns* "nil"))))
      (is-true authors)
      (is (= (length authors) 1))
      (is (= (length (dom:child-nodes (elt authors 0))) 0))
      (is-true events)
      (is (= (length events) 1))
      (is (= (length (dom:child-nodes (elt events 0))) 0))
      (is-true country)
      (is (= (length country) 1))
      (is (= (length (dom:child-nodes (elt country 0))) 0))
      (is-true poem)
      (is (= (length poem) 1))
      (is (= (length (dom:child-nodes (elt poem 0))) 0))
      (is-true ballad)
      (is (= (length ballad) 1))
      (is (= (length (dom:child-nodes (elt ballad 0))) 0))
      (is-true language)
      (is (= (length language) 1))
      (is (= (length (dom:child-nodes (elt language 0))) 0))
      (is-true rdf-nil)
      (is (= (length rdf-nil) 1))
      (is (= (length (dom:child-nodes (elt rdf-nil 0))) 0)))))


(test test-collection
  "Tests a collection that has be exported as a construct of rdf:first,
   rdf:rest and rdf:nil."
  (with-fixture rdf-exporter-test-db ()
    (let ((goethes (get-resources-by-uri "http://some.where/author/Goethe")))
      (let ((wrote-goethe
	     (loop for item across (dom:child-nodes (elt goethes 0))
		when (let ((node-ns (dom:namespace-uri item))
			   (node-name (rdf-importer::get-node-name item)))
		       (and (string= node-ns *sw-arc*)
			    (string= node-name "wrote")))
		return item)))
	(let ((id-1 (rdf-importer::get-ns-attribute wrote-goethe"nodeID")))
	  (is-true id-1)
	  (let ((node-1s (get-resources-by-id id-1)))
	    (is (= (length node-1s) 1))
	    (is (= (length (dom:child-nodes (elt node-1s 0))) 2))
	    (is-true (property-p (elt node-1s 0) *rdf-ns* "first"
				 :resource
				 "http://some.where/poem/Der_Zauberlehrling"))
	    (let ((rest-arc-1
		   (loop for item across (dom:child-nodes (elt node-1s 0))
		      when (let ((node-ns (dom:namespace-uri item))
				 (node-name (rdf-importer::get-node-name item))
				 (nodeID (rdf-importer::get-ns-attribute
					  item "nodeID")))
			     (and (string= node-ns *rdf-ns*)
				  (string= node-name "rest")
				  nodeID))
		      return item)))
	      (is-true rest-arc-1)
	      (let ((id-2 (rdf-importer::get-ns-attribute rest-arc-1 "nodeID")))
		(let ((node-2s (get-resources-by-id id-2)))
		  (is (= (length node-2s) 1))
		  (is (= (length (dom:child-nodes (elt node-2s 0))) 2))
		  (is-true (property-p
			    (elt node-2s 0) *rdf-ns* "first"
			    :resource
			    "http://some.where/ballad/Der_Erlkoenig"))		  
		  (let ((rest-arc-2
			 (loop for item across (dom:child-nodes (elt node-2s 0))
			    when (let ((node-ns (dom:namespace-uri item))
				       (node-name (rdf-importer::get-node-name item))
				       (nodeID (rdf-importer::get-ns-attribute
						item "nodeID")))
				   (and (string= node-ns *rdf-ns*)
					(string= node-name "rest")
					nodeID))
		      return item)))
		    (is-true rest-arc-2)
		    (let ((id-3 (rdf-importer::get-ns-attribute rest-arc-2
								"nodeID")))
		      (let ((node-3s (get-resources-by-id id-3)))
			(is (= (length node-3s) 1))
			(is (= (length (dom:child-nodes (elt node-3s 0))) 2))
			(is-true (property-p
				  (elt node-3s 0) *rdf-ns* "first"
				  :resource
				  "http://some.where/poem/Prometheus"))
			(is-true
			 (property-p 
			  (elt node-3s 0) *rdf-ns* "rest"
			  :resource
			  (concat *rdf-ns* "nil")))))))))))))))


(test test-association
  "Tests a TM association with four roles and one item-identifier."
  (with-fixture rdf-exporter-test-db ()
    (let ((assoc-id (elephant::oid
		     (d:identified-construct 
		      (elephant:get-instance-by-value 
		       'd:ItemIdentifierC 'd:uri
		       "http://some.where/test-association")))))
      (is-true assoc-id)
      (let ((assocs (get-resources-by-id
		     (concat "id_" (write-to-string assoc-id)))))
	(is (= (length assocs)))
	(let ((me (elt assocs 0)))
	  (is (= (length (dom:child-nodes me)) 7))
	  (is-true (type-p me (concat *tm2rdf-ns* "types/Association")))
	  (is-true (identifier-p me "http://some.where/test-association"))
	  (is-true (property-p me *tm2rdf-ns* "associationtype"
			       :resource (concat
					  *sw-arc* "associatedWithEachOther")))
	  (is-true (role-p me "http://some.where/roletype/writer"
			   nil :player-uri "http://some.where/author/Goethe"))

	  (let ((schiller-id (concat
			      "id_"
			      (write-to-string
			       (elephant::oid
				(d:parent
				 (elephant:get-instance-by-value
				  'd:OccurrenceC 'd:charvalue
				  "http://de.wikipedia.org/wiki/Schiller")))))))
	    (is-true (role-p me "http://some.where/roletype/writer"
			     nil :player-id schiller-id)))
	  (is-true (role-p me "http://some.where/roletype/literature"
			   nil :player-uri "http://some.where/types/Poem"))
	  (is-true (role-p me "http://some.where/roletype/literature"
			   (list "http://some.where/test-role")
			   :player-uri "http://some.where/types/Ballad")))))))




(defun run-rdf-exporter-tests()
  "Runs all test cases of this suite."
  (when elephant:*store-controller*
    (elephant:close-store))
  (it.bese.fiveam:run! 'test-resources)
  (it.bese.fiveam:run! 'test-goethe)
  (it.bese.fiveam:run! 'test-erlkoenig)
  (it.bese.fiveam:run! 'test-prometheus)
  (it.bese.fiveam:run! 'test-zauberlehrling)
  (it.bese.fiveam:run! 'test-frankfurt)
  (it.bese.fiveam:run! 'test-weimar)
  (it.bese.fiveam:run! 'test-berlin)
  (it.bese.fiveam:run! 'test-region)
  (it.bese.fiveam:run! 'test-city-and-metropolis)
  (it.bese.fiveam:run! 'test-germany)
  (it.bese.fiveam:run! 'test-german)
  (it.bese.fiveam:run! 'test-born-event)
  (it.bese.fiveam:run! 'test-died-event)
  (it.bese.fiveam:run! 'test-dateRange-zauberlehrling)
  (it.bese.fiveam:run! 'test-dateRange-erlkoenig)
  (it.bese.fiveam:run! 'test-dateRange-prometheus)
  (it.bese.fiveam:run! 'test-schiller)
  (it.bese.fiveam:run! 'test-single-nodes)
  (it.bese.fiveam:run! 'test-collection)
  (it.bese.fiveam:run! 'test-association))