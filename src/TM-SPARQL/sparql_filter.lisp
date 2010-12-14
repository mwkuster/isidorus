;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(in-package :TM-SPARQL)

(defun parse-filter (query-string query-object)
  "A helper functions that returns a filter and the next-query string
   in the form (:next-query string :filter object)."
  (declare (String query-string)
	   (SPARQL-Query query-object))
  ;;TODO: implement
  ;; *replace () by (progn )
  ;; *replace ', """, ''' by "
  ;; *replace !x by (not x)
  ;; *replace +x by (1+ x)
  ;; *replace -x by (1- x)
  ;; *replace x operator y by (filter-operator x y)
  ;;   *=, !=, <, >, <=, >=, +, -, *, /, ||, &&
  ;; *replace function(x), function(x, y), function(x, y, z)
  ;;   by filter-function(x), (filter-function(x, y), filter-function(x, y, z)
  ;; *create and store this filter object
  )

(defun find-literal-end (query-string delimiter &optional (overall-pos 0))
  "Returns the end of the literal corresponding to the passed delimiter
   string. The query-string must start after the opening literal delimiter.
   The return value is an int that represents the start index of closing
   delimiter. delimiter must be either \", ', or '''.
   If the returns value is nil, there is no closing delimiter."
  (declare (String query-string delimiter)
	   (Integer overall-pos))
  (let ((current-pos (search delimiter query-string)))
    (if current-pos
	(if (string-ends-with (subseq query-string 0 current-pos) "\\")
	    (find-literal-end (subseq query-string (+ current-pos
						      (length delimiter)))
			      delimiter (+ overall-pos current-pos 1))
	    (+ overall-pos current-pos (length delimiter)))
	nil)))