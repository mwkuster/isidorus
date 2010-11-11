;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :trivial-queries-test
  (:use  :cl
	 :it.bese.FiveAM
	 :datamodel
	 :unittests-constants
	 :fixtures
	 :constants)
  (:export :run-trivial-queries-tests
	   :trivial-queries-tests
	   :test-aka
	   :test-isa
	   :test-x-by-value
	   :test-x-by-type
	   :test-invoke-on
	   :test-instance-of
	   :test-supertypes
	   :test-direct-instance-of
	   :test-direct-supertypes
	   :test-supertype-associations
	   :test-instance-of-associations
	   :test-associations-of
	   :test-roles-by-type
	   :test-roles-by-player
	   :test-filter-associations-by-type
	   :test-filter-associations-by-role))


(in-package :trivial-queries-test)


(def-suite trivial-queries-tests
     :description "tests various key functions of the trivial-query-test of
                   the datamodel module")

(in-suite trivial-queries-tests)

(test test-aka
  "Tests the function aka."
  (with-fixture with-tm-filled-db ("data_base" *poems.xtm*)
    (with-revision 0
      (let ((region (get-item-by-id "region"))
	    (city (get-item-by-id "city"))
	    (poem (get-item-by-id "poem"))
	    (supertype (get-item-by-psi *supertype-psi*))
	    (subtype (get-item-by-psi *subtype-psi*))
	    (supertype-subtype (get-item-by-psi *supertype-subtype-psi*))
	    (rev (get-revision)))
	(is-true region)
	(is-true city)
	(is-true poem)
	(is-true supertype)
	(is-true subtype)
	(is-true supertype-subtype)
	(is-true (aka city region))
	(is-false (aka city city))
	(make-construct 'AssociationC
			:start-revision rev
			:instance-of supertype-subtype
			:roles (list (list :start-revision rev
					   :player region
					   :instance-of subtype)
				     (list :start-revision rev
					   :player poem
					   :instance-of supertype)))
	(is-true (aka city region))
	(is-true (aka city poem))
	(is-true (aka region poem))))))


(test test-isa
  "Tests the function isa."
  (with-fixture with-tm-filled-db ("data_base" *poems.xtm*)
    (with-revision 0
      (let ((region (get-item-by-id "region"))
	    (metropolis (get-item-by-id "metropolis"))
	    (poem (get-item-by-id "poem"))
	    (frankfurt (get-item-by-id "frankfurt_am_main")))
	(is-true region)
	(is-true frankfurt)
	(is-true metropolis)
	(is-true poem)
	(is-true (isa frankfurt metropolis))
	(is-true (isa frankfurt region))))))


(test test-x-by-value
  "Tests the functions names-by-value, occurrences-by-value
   and characteristics-by-value."
  (with-fixture with-tm-filled-db ("data_base" *poems.xtm*)
    (with-revision 0
      (let ((goethe (get-item-by-id "goethe"))
	    (poem (get-item-by-id "poem"))
	    (fn "Johann Wolfgang")
	    (ln "von Goethe")
	    (ai "http://de.wikipedia.org/wiki/Johann_Wolfgang_von_Goethe")
	    (as "any string"))
	(let ((fun-fn (lambda(value)
			(string= value fn)))
	      (fun-ln (lambda(value)
			(string= value ln)))
	      (fun-ai (lambda(value)
			(string= value ai)))
	      (fun-as (lambda(value)
			(string= value as))))
	  (is-true goethe)
	  (is-true poem)
	  (is-false (names-by-value goethe fun-as))
	  (is-false (occurrences-by-value goethe fun-as))
	  (is-false (characteristics-by-value goethe fun-as))
	  (is (= (length (names-by-value goethe fun-fn)) 1))
	  (is (= (length (names-by-value goethe fun-ln)) 1))
	  (is (= (length (occurrences-by-value goethe fun-ai)) 1))
	  (is (string= (charvalue (first (names-by-value goethe fun-fn)))
		       fn))
	  (is (string= (charvalue (first (names-by-value goethe fun-ln)))
		       ln))
	  (is (string= (charvalue (first (occurrences-by-value goethe fun-ai)))
		       ai))
	  (is (= (length (characteristics-by-value goethe fun-fn)) 1))
	  (is (string=
	       (charvalue (first (characteristics-by-value goethe fun-fn)))
	       fn)))))))


(test test-x-by-type
  "Tests the functions names-by-type, occurrences-by-type
   and characteristics-by-type."
  (with-fixture with-tm-filled-db ("data_base" *poems.xtm*)
    (with-revision 0
      (let ((goethe (get-item-by-id "goethe"))
	    (first-name (get-item-by-id "first-name"))
	    (last-name (get-item-by-id "last-name"))
	    (author-info (get-item-by-id "author-info"))
	    (poem (get-item-by-id "poem")))
	(is-true goethe)
	(is-true first-name)
	(is-true last-name)
	(is-true author-info)
	(is-true poem)
	(is-false (names-by-type goethe poem))
	(is-false (occurrences-by-type goethe poem))
	(is-false (characteristics-by-type goethe poem))
	(is (= (length (names-by-type goethe first-name)) 1))
	(is (= (length (names-by-type goethe last-name)) 1))
	(is (= (length (occurrences-by-type goethe author-info)) 1))
	(is (string= (charvalue (first (names-by-type goethe first-name)))
		     "Johann Wolfgang"))
	(is (string= (charvalue (first (names-by-type goethe last-name)))
		     "von Goethe"))
	(is (string=
	     (charvalue (first (occurrences-by-type goethe author-info)))
	     "http://de.wikipedia.org/wiki/Johann_Wolfgang_von_Goethe"))
	(is (= (length (characteristics-by-type goethe first-name)) 1))
	(is (string=
	     (charvalue (first (characteristics-by-type goethe first-name)))
	     "Johann Wolfgang"))))))


(test test-invoke-on
  "Tests the function invoke-on."
  (with-fixture with-tm-filled-db ("data_base" *poems.xtm*)
    (with-revision 0
      (let ((frankfurt (get-item-by-id "frankfurt_am_main")))
	(is-true frankfurt)
	(is (= (length (occurrences frankfurt)) 1))
	(is (= (invoke-on (first (occurrences frankfurt))
			  #'(lambda(value)
			      (+ 1 (parse-integer value))))
	       (+ 1 659021)))))))



(test test-instance-of
  "Tests the function instance-of."
  (with-fixture with-tm-filled-db ("data_base" *poems.xtm*)
    (with-revision 0
      (let ((region (get-item-by-id "region"))
	    (metropolis (get-item-by-id "metropolis"))
	    (poem (get-item-by-id "poem"))
	    (frankfurt (get-item-by-id "frankfurt_am_main")))
	(is-true region)
	(is-true frankfurt)
	(is-true metropolis)
	(is-true poem)
	(is (= (length (instance-of frankfurt)) 2))
	(is-false (set-exclusive-or (instance-of frankfurt)
				    (list metropolis region)))))))


(test test-supertypes
  "Tests the function supertypes."
  (with-fixture with-tm-filled-db ("data_base" *poems.xtm*)
    (with-revision 0
      (let ((region (get-item-by-id "region"))
	    (city (get-item-by-id "city"))
	    (poem (get-item-by-id "poem"))
	    (supertype (get-item-by-psi *supertype-psi*))
	    (subtype (get-item-by-psi *subtype-psi*))
	    (supertype-subtype (get-item-by-psi *supertype-subtype-psi*))
	    (rev (get-revision)))
	(is-true region)
	(is-true city)
	(is-true poem)
	(is-true supertype)
	(is-true subtype)
	(is-true supertype-subtype)
	(is (= (length (supertypes city)) 1))
	(is (eql (first (supertypes city)) region))
	(is-false (supertypes region))
	(make-construct 'AssociationC
			:start-revision rev
			:instance-of supertype-subtype
			:roles (list (list :start-revision rev
					   :player region
					   :instance-of subtype)
				     (list :start-revision rev
					   :player poem
					   :instance-of supertype)))
	(is (= (length (supertypes city)) 2))))))


(test test-direct-instance-of
  "Tests the function direct-instance-of."
  (with-fixture with-tm-filled-db ("data_base" *poems.xtm*)
    (with-revision 0
      (let ((region (get-item-by-id "region"))
	    (frankfurt (get-item-by-id "frankfurt_am_main"))
	    (metropolis (get-item-by-id "metropolis")))
	(is-true region)
	(is-true metropolis)
	(is-true frankfurt)
	(is (= (length (direct-instance-of frankfurt)) 1))
	(is (eql (first (direct-instance-of frankfurt)) metropolis))
	(is-false (direct-instance-of metropolis))))))


(test test-direct-supertypes
  "Tests the function direct-supertypes."
  (with-fixture with-tm-filled-db ("data_base" *poems.xtm*)
    (with-revision 0
      (let ((region (get-item-by-id "region"))
	    (city (get-item-by-id "city"))
	    (poem (get-item-by-id "poem"))
	    (supertype (get-item-by-psi *supertype-psi*))
	    (subtype (get-item-by-psi *subtype-psi*))
	    (supertype-subtype (get-item-by-psi *supertype-subtype-psi*))
	    (rev (get-revision)))
	(is-true region)
	(is-true city)
	(is-true poem)
	(is-true supertype)
	(is-true subtype)
	(is-true supertype-subtype)
	(is (= (length (direct-supertypes city)) 1))
	(is (eql (first (direct-supertypes city)) region))
	(is-false (direct-supertypes region))
	(make-construct 'AssociationC
			:start-revision rev
			:instance-of supertype-subtype
			:roles (list (list :start-revision rev
					   :player region
					   :instance-of subtype)
				     (list :start-revision rev
					   :player poem
					   :instance-of supertype)))
	(is (= (length (direct-supertypes city)) 1))))))


(test test-supertype-associations
  "Tests the function supertype-associations."
  (with-fixture with-tm-filled-db ("data_base" *poems.xtm*)
    (with-revision 0
      (let ((region (get-item-by-id "region"))
	    (city (get-item-by-id "city"))
	    (metropolis (get-item-by-id "metropolis"))
	    (assocs (get-all-associations))
	    (supertype (get-item-by-psi *supertype-psi*))
	    (subtype (get-item-by-psi *subtype-psi*))
	    (supertype-subtype (get-item-by-psi *supertype-subtype-psi*)))
	(is-true region)
	(is-true city)
	(is-true metropolis)
	(is-true supertype)
	(is-true subtype)
	(is-true supertype-subtype)
	(let ((assoc-city
	       (find-if
		#'(lambda(assoc)
		    (and (eql (instance-of assoc) supertype-subtype)
			 (find-if #'(lambda(role)
				      (and (eql (player role) city)
					   (eql (instance-of role) subtype)))
				  (roles assoc))
			 (find-if #'(lambda(role)
				      (and (eql (player role) region)
					   (eql (instance-of role) supertype)))
				  (roles assoc))))
		assocs))
	      (assoc-metropolis
	       (find-if
		#'(lambda(assoc)
		    (and (eql (instance-of assoc) supertype-subtype)
			 (find-if #'(lambda(role)
				      (and (eql (player role) metropolis)
					   (eql (instance-of role) subtype)))
				  (roles assoc))
			 (find-if #'(lambda(role)
				      (and (eql (player role) region)
					   (eql (instance-of role) supertype)))
				  (roles assoc))))
		assocs)))
	  (is-true assoc-city)
	  (is-true assoc-metropolis)
	  (is (= (length (supertype-associations city)) 1))
	  (is (= (length (supertype-associations metropolis)) 1))
	  (is (eql (first (supertype-associations city)) assoc-city))
	  (is (eql (first (supertype-associations metropolis)) assoc-metropolis))
	  (is-false (supertype-associations region)))))))


(test test-instance-of-associations
  "Tests the function instance-of-associations."
  (with-fixture with-tm-filled-db ("data_base" *poems.xtm*)
    (with-revision 0
      (let ((goethe (get-item-by-id "goethe"))
	    (instance (get-item-by-psi *instance-psi*))
	    (type (get-item-by-psi *type-psi*))
	    (type-instance (get-item-by-psi *type-instance-psi*))
	    (author (get-item-by-id "author")))
	(is-true goethe)
	(is-true instance)
	(is-true type)
	(is-true type-instance)
	(is-true author)
	(is (= (length (instance-of-associations goethe)) 1))
	(is (eql type-instance
		 (instance-of (first (instance-of-associations goethe)))))
	(is-true (filter-associations-by-role (instance-of-associations goethe)
					      instance goethe))
	(is-true (filter-associations-by-role (instance-of-associations goethe)
					      type author))
	(is-true (filter-associations-by-type (instance-of-associations goethe)
					      type-instance))))))


(test test-associations-of
  "Tests the function associations-of."
  (with-fixture with-tm-filled-db ("data_base" *poems.xtm*)
    (with-revision 0
      (let ((goethe (get-item-by-id "goethe"))
	    (writer (get-item-by-id "writer"))
	    (written-by (get-item-by-id "written-by"))
	    (written (get-item-by-id "written"))
	    (erlkoenig (get-item-by-id "erlkoenig"))
	    (instance (get-item-by-psi *instance-psi*))
	    (poem (get-item-by-id "poem")))
	(is-true goethe)
	(is-true writer)
	(is-true written-by)
	(is-true written)
	(is-true erlkoenig)
	(is-true instance)
	(is-true poem)
	(is (= (length (associations-of goethe nil nil nil nil)) 4))
	(is (= (length (associations-of goethe writer nil nil nil)) 3))
	(is (= (length (associations-of goethe writer written-by nil nil)) 2))
	(is (= (length (associations-of goethe writer written-by written nil)) 2))
	(is (= (length (associations-of goethe writer written-by written erlkoenig)) 1))
	(is-false (associations-of goethe writer written-by written instance))
	(is-false (associations-of goethe writer written-by instance erlkoenig))
	(is (= (length (associations-of goethe instance nil nil nil)) 1))
	(is-false (associations-of goethe writer written-by written erlkoenig
				   :other-role-player-is-type t))
	(is (= (length (associations-of  goethe writer written-by written poem
					 :other-role-player-is-type t)) 2))))))


(test test-roles-by-type
  "Tests the function roles-by-type bound to TopicC and AssociationC."
  (with-fixture with-tm-filled-db ("data_base" *poems.xtm*)
    (with-revision 0
      (let ((goethe (get-item-by-id "goethe"))
	    (writer (get-item-by-id "writer"))
	    (written (get-item-by-id "written"))
	    (instance (get-item-by-psi *instance-psi*))
	    (assoc (get-item-by-item-identifier "written-by-erlkoenig-goethe")))
	(is-true goethe)
	(is-true writer)
	(is-true written)
	(is-true instance)
	(is-true assoc)
	(is (= (length (roles-by-type goethe writer)) 3))
	(is (= (length (roles-by-type goethe nil)) 4))
	(is (= (length (roles-by-type goethe instance)) 1))
	(is-false (roles-by-type goethe written))
	(is (= (length (roles-by-type assoc writer)) 1))
	(is (eql writer (instance-of (first (roles-by-type assoc writer)))))
	(is (= (length (roles-by-type assoc nil)) 2))))))


(test test-roles-by-player
  "Tests the function roles-by-player."
  (with-fixture with-tm-filled-db ("data_base" *poems.xtm*)
    (with-revision 0
      (let ((goethe (get-item-by-id "goethe"))
	    (writer (get-item-by-id "writer"))
	    (written (get-item-by-id "written"))
	    (instance (get-item-by-psi *instance-psi*))
	    (assoc (get-item-by-item-identifier "written-by-erlkoenig-goethe"))
	    (author (get-item-by-id "author")))
	(is-true goethe)
	(is-true author)
	(is-true writer)
	(is-true written)
	(is-true instance)
	(is-true assoc)
	(is (= (length (roles-by-player assoc goethe)) 1))
	(is (eql goethe (player (first (roles-by-player assoc goethe)))))
	(is (= (length (roles-by-player assoc written)) 0))
	(is (= (length (roles-by-player assoc nil)) 2))
	(is (= (length (roles-by-player assoc author :role-player-is-type t))
	       1))
	(is-false (roles-by-player assoc goethe :role-player-is-type t))
	(is (eql goethe (player (first (roles-by-player
					assoc author
					:role-player-is-type t)))))))))


(test test-filter-associations-by-type
  "Tests the function roles-by-player."
  (with-fixture with-tm-filled-db ("data_base" *poems.xtm*)
    (with-revision 0
      (let ((written-by (get-item-by-id "written-by"))
	    (born-in (get-item-by-id "born-in"))
	    (assocs (get-all-associations)))
	(is-true written-by)
	(is-true assocs)
	(is-true born-in)
	(is (= (length (filter-associations-by-type assocs written-by)) 4))
	(is (> (length (filter-associations-by-type assocs nil)) (+ 4 2)))
	(is (= (length (filter-associations-by-type assocs born-in)) 2))))))


(test test-filter-associations-by-role
  "Tests the function roles-by-player."
  (with-fixture with-tm-filled-db ("data_base" *poems.xtm*)
    (with-revision 0
      (let ((written-by (get-item-by-id "written-by"))
	    (born-in (get-item-by-id "born-in"))
	    (written (get-item-by-id "written"))
	    (writer (get-item-by-id "writer"))
	    (place (get-item-by-id "place"))
	    (goethe (get-item-by-id "goethe"))
	    (frankfurt (get-item-by-id "frankfurt_am_main"))
	    (assocs (get-all-associations))
	    (author (get-item-by-id "author")))
	(is-true written-by)
	(is-true assocs)
	(is-true born-in)
	(is-true author)
	(is-true written)
	(is-true writer)
	(is-true place)
	(is-true frankfurt)
	(is (= (length (filter-associations-by-role assocs place frankfurt)) 1))
	(is (= (length (filter-associations-by-role assocs written nil)) 4))
	(is (= (length (filter-associations-by-role assocs written goethe)) 2))
	(is (= (length (filter-associations-by-role assocs writer nil)) 6))
	(is (= (length (filter-associations-by-role assocs nil goethe)) 4))
	(is (> (length (filter-associations-by-role assocs nil nil)) (+ 4 3)))
	(is-false (filter-associations-by-role assocs writer goethe
					       :role-player-is-type t))
	(is (= (length (filter-associations-by-role assocs writer author
						    :role-player-is-type t))
	       6))))))
	




(defun run-trivial-queries-tests ()
  (it.bese.fiveam:run! 'trivial-queries-test:trivial-queries-tests))