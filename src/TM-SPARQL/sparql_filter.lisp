;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(in-package :TM-SPARQL)


(defparameter *supported-functions*
  (list "BOUND" "isLITERAL" "STR" "DATATYPE" "REGEX")
  "Contains all supported SPARQL-functions")


(defparameter *supported-primary-arithmetic-operators*
  (list "*" "/") "Contains all supported arithmetic operators.")


(defparameter *supported-secundary-arithmetic-operators*
  (list "+" "-") "Contains all supported arithmetic operators.")


(defparameter *supported-compare-operators*
  (list "!=" "<=" ">=" "=" "<" ">") ;note the order is important!
                                    ;the operators with length = 2
                                    ;must be listed first
  "Contains all supported binary operators.")


(defparameter *supported-join-operators*
  (list "||" "&&") "Contains all supported join operators.")


(defparameter *supported-unary-operators*
  (list "!" "+" "-") "Contains all supported unary operators")


(defparameter *allowed-filter-calls*
  (append (list "one+" "one-" "progn" "or" "and" "not" "/=" "="
		">" ">=" "<" "<=" "+" "-" "*" "/")
	  *supported-functions*))


(defun *2-compare-operators* ()
  (remove-null
   (map 'list #'(lambda(op)
		  (when (= (length op) 2)
		    op))
	*supported-compare-operators*)))


(defun *1-compare-operators* ()
  (remove-null
   (map 'list #'(lambda(op)
		  (when (= (length op) 1)
		    op))
	*supported-compare-operators*)))


(defun *supported-arithmetic-operators* ()
  (append *supported-primary-arithmetic-operators*
	  *supported-secundary-arithmetic-operators*))


(defun *supported-binary-operators* ()
  (append (*supported-arithmetic-operators*)
	  *supported-compare-operators*
	  *supported-join-operators*))


(defun *supported-operators* ()
  (union (*supported-binary-operators*) *supported-unary-operators*
	 :test #'string=))


(defparameter *supported-brackets*
  (list "(" ")")
  "Contains all supported brackets in a list of strings.")


(defun make-sparql-parser-condition(rest-of-query entire-query expected)
  "Creates a spqrql-parser-error object."
  (declare (String rest-of-query entire-query expected))
  (let ((message
	 (format nil "The query:~%\"~a\"~%~%has a bad token at position ~a => ~a.~%Expected: ~a"
		 entire-query (- (length entire-query)
				 (length rest-of-query))
		 (subseq entire-query (- (length entire-query)
					 (length rest-of-query)))
		 expected)))
    (make-condition 'sparql-parser-error :message message)))


(defgeneric parse-filter (construct query-string)
  (:documentation "A helper functions that returns a filter and the next-query
                   string in the form (:next-query string
                   :filter-string object).")
  (:method ((construct SPARQL-Query) (query-string String))
    ;note the order of the invacations is important!
    (let* ((result-set-boundings (set-boundings construct query-string))
	   (filter-string (getf result-set-boundings :filter-string))
	   (next-query (getf result-set-boundings :next-query))
	   (original-filter-string
	    (subseq query-string 0 (- (length query-string)
				      (length next-query))))
	   (filter-string-unary-ops
	    (set-unary-operators construct filter-string))
	   (filter-string-or-and-ops
	    (set-or-and-operators construct filter-string-unary-ops
				  original-filter-string))
	   (filter-string-arithmetic-ops
	    (set-arithmetic-operators construct filter-string-or-and-ops))
	   (filter-string-compare-ops
	    (set-compare-operators construct filter-string-arithmetic-ops))
	   (filter-string-functions
	    (set-functions construct filter-string-compare-ops)))
      (add-filter construct
		  (scan-filter-for-deprecated-calls
		   construct filter-string-functions original-filter-string))
      (parse-group construct next-query))))
  ;;TODO: implement
  ;; *add ^^datatype to the object-literal-results
  ;; *implement to-literal => CharacteristicC => \"...\"^^datatype => use for tm-sparql
  ;; *implement str correctly


(defgeneric scan-filter-for-deprecated-calls (construct filter-string
							original-filter)
  (:documentation "Returns the passed filter-string where all functions
                   are explicit wrapped in the filter-functions package
                   or throws a sparql-parser-error of there is an
                   unallowed function call.")
  (:method ((construct SPARQL-Query) (filter-string String)
	    (original-filter String))
    (let ((result ""))
      (dotimes (idx (length filter-string) result)
	(let ((fun-name (return-function-name (subseq filter-string idx))))
	  (cond ((not fun-name)
		 (push-string (subseq filter-string idx (1+ idx)) result))
		((string-starts-with-one-of fun-name *allowed-filter-calls*)
		 (push-string "(filter-functions::" result)
		 (push-string fun-name result)
		 (incf idx (length fun-name)))
		(t
		 (error 
		  (make-condition
		   'exceptions:sparql-parser-error
		   :message (format nil "Invalid filter: the filter \"~a\" evaluated to \"~a\" which contains the deprecated function ~a!"
				    filter-string original-filter fun-name))))))))))


(defun return-function-name (filter-string)
  "If the string starts with ( there is returned the function name
   that is placed directly after the (."
  (declare (String filter-string))
  (when (string-starts-with filter-string "(")
    (let ((local-str (trim-whitespace-left (subseq filter-string 1)))
	  (whitespaces (map 'list #'string (white-space)))
	  (result ""))
      (dotimes (idx (length local-str) result)
	(let ((current-char (subseq local-str idx (1+ idx))))
	  (if (string-starts-with-one-of
	       current-char (append whitespaces *supported-brackets*))
	      (setf idx (length local-str))
	      (push-string current-char result)))))))


(defgeneric set-functions (construct filter-string)
  (:documentation "Transforms all supported functions of the form
                   function(x, y) to (function x y).")
  (:method ((construct SPARQL-Query) (filter-string String))
    (let ((op-pos (find-functions filter-string)))
      (if (not op-pos)
	  filter-string
	  (let* ((fun-name
		  (return-if-starts-with (subseq filter-string op-pos)
					 *supported-functions*))
		 (left-str (subseq filter-string 0 op-pos))
		 (right-str (subseq filter-string
				    (+ op-pos (length fun-name))))
		 (cleaned-right-str (trim-whitespace-left right-str))
		 (arg-list (bracket-scope cleaned-right-str))
		 (cleaned-arg-list (clean-function-arguments arg-list))
		 (modified-str
		  (concatenate
		   'string left-str "(" fun-name " " cleaned-arg-list ")"
		   (subseq right-str (+ (- (length right-str)
					   (length cleaned-right-str))
					(length arg-list))))))
	    (set-functions construct modified-str))))))


(defun clean-function-arguments (argument-string)
  "Transforms all arguments within an argument list of the form
   (x, y, z, ...) to x y z."
  (declare (String argument-string))
  (when (and (string-starts-with argument-string "(")
	     (string-ends-with argument-string ")"))
    (let ((local-str (subseq argument-string 1 (1- (length argument-string))))
	  (result ""))
      (dotimes (idx (length local-str) result)
	(let ((current-char (subseq local-str idx (1+ idx))))
	  (if (and (string= current-char ",")
		   (not (in-literal-string-p local-str idx)))
	      (push-string " " result)
	      (push-string current-char result)))))))


(defun find-functions (filter-string)
  "Returns the idx of the first found 'BOUND', 'isLITERAL', 'STR',
   'DATATYPE', or 'REGEX'.
   It must not be in a literal string or directly after a (."
  (declare (String filter-string))
  (let* ((first-pos
	  (search-first-ignore-literals *supported-functions*
					filter-string)))
    (when first-pos
      (let ((left-part (trim-whitespace-right (subseq filter-string 0 first-pos))))
	(if (not (string-ends-with left-part "("))
	    first-pos
	    (let ((next-pos
		   (find-functions (subseq filter-string (1+ first-pos)))))
	      (when next-pos
		(+ 1 first-pos next-pos))))))))


(defgeneric set-compare-operators (construct filter-string)
  (:documentation "Transforms the =, !=, <, >, <= and >= operators in the
                   filter string to the the corresponding lisp functions.")
  (:method ((construct SPARQL-Query) (filter-string String))
    (let ((op-pos (find-compare-operators filter-string)))
      (if (not op-pos)
	  filter-string
	  (let* ((op-str (if (string-starts-with-one-of
			      (subseq filter-string op-pos)
			      (*2-compare-operators*))
			     (subseq filter-string op-pos (+ 2 op-pos))
			     (subseq filter-string op-pos (1+ op-pos))))
		 (left-str (subseq filter-string 0 op-pos))
		 (right-str (subseq filter-string (+ (length op-str) op-pos)))
		 (left-scope (find-compare-left-scope left-str))
		 (right-scope (find-compare-right-scope right-str))
		 (modified-str
		  (concatenate
		   'string (subseq left-str 0 (- (length left-str)
						 (length left-scope)))
		   "(" op-str " " left-scope " " right-scope ")"
		   (subseq right-str (length right-scope)))))
	    (set-compare-operators construct modified-str))))))


(defun find-compare-operators (filter-string)
  "Returns the idx of the first found =, !=, <, >, <= or >= operator.
   It must not be in a literal string or directly after a (."
  (declare (String filter-string))
  (let* ((first-pos
	  (search-first-ignore-literals *supported-compare-operators*
					filter-string))
	 (delta (if first-pos
		    (if (string-starts-with-one-of
			 (subseq filter-string first-pos)
			 (*2-compare-operators*))
			2
			1)
		    1)))
    (when first-pos
      (let ((left-part (trim-whitespace-right (subseq filter-string 0 first-pos))))
	(if (not (string-ends-with-one-of
		  left-part (append (*1-compare-operators*) (list "("))))
	    first-pos
	    (let ((next-pos
		   (find-compare-operators (subseq filter-string (+ delta first-pos)))))
	      (when next-pos
		(+ delta first-pos next-pos))))))))


(defun find-compare-left-scope (left-string)
  "Returns the string that is the left part of the binary scope."
  (declare (String left-string))
  (let* ((first-bracket
	  (let ((inner-value (search-first-unclosed-paranthesis left-string)))
	    (when inner-value
	      (+ inner-value (1+ (length (name-after-paranthesis
					  (subseq left-string inner-value))))))))
	 (paranthesis-pair-idx
	  (let* ((cleaned-str (trim-whitespace-right left-string))
		 (bracket-scope (reverse-bracket-scope cleaned-str)))
	    (when bracket-scope
	      (- (- (length left-string)
		    (- (length left-string) (length cleaned-str)))
		 (length bracket-scope)))))
	 (start-idx (or first-bracket paranthesis-pair-idx 0)))
    (subseq left-string start-idx)))


(defun find-compare-right-scope (right-string)
  "Returns the string that is the right part of the binary scope."
  (declare (String right-string))
  (let* ((first-pos
	  (search-first-ignore-literals *supported-compare-operators*
					right-string))
	 (first-bracket
	  (let ((inner-value (search-first-unopened-paranthesis right-string)))
	    (when inner-value (1+ inner-value))))
	 (paranthesis-pair-idx
	  (let* ((cleaned-str (trim-whitespace-left right-string))
		 (bracket-scope (bracket-scope cleaned-str)))
	    (when bracket-scope
	      (+ (- (length right-string) (length cleaned-str))
		 (length bracket-scope)))))
	 (end-idx (cond (paranthesis-pair-idx
			 paranthesis-pair-idx)
			((and first-pos first-bracket)
			 (min first-pos first-bracket))
			(first-pos first-pos)
			(first-bracket first-bracket)
			(t (if (= (length right-string) 0)
			       0
			       (length right-string))))))
    (subseq right-string 0 end-idx)))


(defgeneric set-arithmetic-operators (construct filter-string)
  (:documentation "Transforms the +, -, *, / operators in the filter
                   string to the the corresponding lisp functions.")
  (:method ((construct SPARQL-Query) (filter-string String))
    (let ((filter-string-*/ (set-*-and-/-operators construct filter-string)))
      (set-+-and---operators construct filter-string-*/))))


(defun find-*/-operators (filter-string)
  "Returns the idx of the first found * or / operator.
   It must not be in a literal string or directly after a (."
  (declare (String filter-string))
  (let ((first-pos
	 (search-first-ignore-literals *supported-primary-arithmetic-operators*
				       filter-string)))
    (when first-pos
      (let ((left-part (trim-whitespace-right (subseq filter-string 0 first-pos))))
	(if (not (string-ends-with left-part "("))
	    first-pos
	    (let ((next-pos
		   (find-*/-operators (subseq filter-string (1+ first-pos)))))
	      (when next-pos
		(+ 1 first-pos next-pos))))))))


(defgeneric set-*-and-/-operators (construct filter-string)
  (:documentation "Transforms the *, / operators in the filter
                   string to the the corresponding lisp functions.")
  (:method ((construct SPARQL-Query) (filter-string String))
    (let ((op-pos (find-*/-operators filter-string)))
      (if (not op-pos)
	  filter-string
	  (let* ((op-str (subseq filter-string op-pos (1+ op-pos)))
		 (left-str (subseq filter-string 0 op-pos))
		 (right-str (subseq filter-string (1+ op-pos)))
		 (left-scope (find-*/-left-scope left-str))
		 (right-scope (find-*/-right-scope right-str))
		 (modified-str
		  (concatenate
		   'string (subseq left-str 0 (- (length left-str)
						 (length left-scope)))
		   "(" op-str " " left-scope " " right-scope ")"
		   (subseq right-str (length right-scope)))))
	    (set-*-and-/-operators construct modified-str))))))


(defun find-*/-left-scope (left-string)
  "Returns the string that is the left part of the binary scope."
  (declare (String left-string))
  (let* ((first-bracket
	  (let ((inner-value (search-first-unclosed-paranthesis left-string)))
	    (when inner-value
	      (+ inner-value (1+ (length (name-after-paranthesis
					  (subseq left-string inner-value))))))))
	 (other-anchor
	  (let ((inner-value
		 (search-first-ignore-literals
		  (append *supported-secundary-arithmetic-operators*
			  *supported-compare-operators*)
		  left-string :from-end t)))
	    (when inner-value
	      (1+ inner-value))))
	 (paranthesis-pair-idx
	  (let* ((cleaned-str (trim-whitespace-right left-string))
		 (bracket-scope (reverse-bracket-scope cleaned-str)))
	    (when bracket-scope
	      (- (- (length left-string)
		    (- (length left-string) (length cleaned-str)))
		 (length bracket-scope)))))
	 (start-idx (cond (paranthesis-pair-idx
			   paranthesis-pair-idx)
			  ((and first-bracket other-anchor)
			   (max first-bracket other-anchor))
			  ((or first-bracket other-anchor)
			   (or first-bracket other-anchor))
			  (t 0))))
    (subseq left-string start-idx)))


(defun find-*/-right-scope (right-string)
  "Returns the string that is the right part of the binary scope."
  (declare (String right-string))
  (let* ((first-pos (search-first-ignore-literals
		     (append (*supported-arithmetic-operators*)
			     *supported-compare-operators*)
		     right-string))
	 (first-bracket
	  (let ((inner-value (search-first-unopened-paranthesis right-string)))
	    (when inner-value (1+ inner-value))))
	 (paranthesis-pair-idx
	  (let* ((cleaned-str (trim-whitespace-left right-string))
		 (bracket-scope (bracket-scope cleaned-str)))
	    (when bracket-scope
	      (+ (- (length right-string) (length cleaned-str))
		 (length bracket-scope)))))
	 (end-idx (cond (paranthesis-pair-idx
			 paranthesis-pair-idx)
			((and first-pos first-bracket)
			 (min first-pos first-bracket))
			(first-pos first-pos)
			(first-bracket first-bracket)
			(t (if (= (length right-string) 0)
			       (1- (length right-string)))))))
    (subseq right-string 0 end-idx)))


(defgeneric set-+-and---operators (construct filter-string)
  (:documentation "Transforms the +, - operators in the filter
                   string to the the corresponding lisp functions.")
  (:method ((construct SPARQL-Query) (filter-string String))
    (let ((op-pos (find-+--operators filter-string)))
      (if (not op-pos)
	  filter-string
	  (let* ((op-str (subseq filter-string op-pos (1+ op-pos)))
		 (left-str (subseq filter-string 0 op-pos))
		 (right-str (subseq filter-string (1+ op-pos)))
		 (left-scope (find-+--left-scope left-str))
		 (right-scope (find-+--right-scope right-str))
		 (modified-str
		  (concatenate
		   'string (subseq left-str 0 (- (length left-str)
						 (length left-scope)))
		   "(" op-str " " left-scope " " right-scope ")"
		   (subseq right-str (length right-scope)))))
	    (set-+-and---operators construct modified-str))))))


(defun find-+--left-scope (left-string)
  "Returns the string that is the left part of the binary scope."
  (declare (String left-string))
  (let* ((first-bracket
	  (let ((inner-value (search-first-unclosed-paranthesis left-string)))
	    (when inner-value
	      (+ inner-value (1+ (length (name-after-paranthesis
					  (subseq left-string inner-value))))))))
	 (other-anchor
	  (let ((inner-value
		 (search-first-ignore-literals *supported-compare-operators*
					       left-string :from-end t)))
	    (when inner-value
	      (1+ inner-value))))
	 (paranthesis-pair-idx
	  (let* ((cleaned-str (trim-whitespace-right left-string))
		 (bracket-scope (reverse-bracket-scope cleaned-str)))
	    (when bracket-scope
	      (- (- (length left-string)
		    (- (length left-string) (length cleaned-str)))
		 (length bracket-scope)))))
	 (start-idx (cond (paranthesis-pair-idx
			   paranthesis-pair-idx)
			  ((and first-bracket other-anchor)
			   (max first-bracket other-anchor))
			  ((or first-bracket other-anchor)
			   (or first-bracket other-anchor))
			  (t 0))))
    (subseq left-string start-idx)))


(defun find-+--right-scope (right-string)
  "Returns the string that is the right part of the binary scope."
  (declare (String right-string))
  (let* ((first-pos (search-first-ignore-literals
		     (append (*supported-arithmetic-operators*)
			     *supported-compare-operators*)
		     right-string))
	 (first-bracket
	  (let ((inner-value (search-first-unopened-paranthesis right-string)))
	    (when inner-value (1+ inner-value))))
	 (paranthesis-pair-idx
	  (let* ((cleaned-str (trim-whitespace-left right-string))
		 (bracket-scope (bracket-scope cleaned-str)))
	    (when bracket-scope
	      (+ (- (length right-string) (length cleaned-str))
		 (length bracket-scope)))))
	 (end-idx (cond (paranthesis-pair-idx
			 paranthesis-pair-idx)
			((and first-pos first-bracket)
			 (min first-pos first-bracket))
			(first-pos first-pos)
			(first-bracket first-bracket)
			(t (if (= (length right-string) 0)
			       (1- (length right-string)))))))
    (subseq right-string 0 end-idx)))


(defun find-+--operators (filter-string)
  "Returns the idx of the first found + or - operator.
   It must not be in a literal string or directly after a (."
  (declare (String filter-string))
  (let ((first-pos
	 (search-first-ignore-literals *supported-secundary-arithmetic-operators*
				       filter-string)))
    (when first-pos
      (let ((left-part (trim-whitespace-right (subseq filter-string 0 first-pos))))
	(if (and (not (string-ends-with left-part "(one"))
		 (not (string-ends-with left-part "(")))
	    first-pos
	    (let ((next-pos
		   (find-+--operators (subseq filter-string (1+ first-pos)))))
	      (when next-pos
		(+ 1 first-pos next-pos))))))))


(defgeneric set-or-and-operators (construct filter-string original-filter-string)
  (:documentation "Transforms the || and && operators in the filter string to
                   the the lisp or and and functions.")
  (:method ((construct SPARQL-Query) (filter-string String)
	    (original-filter-string String))
    (let ((op-pos (search-first-ignore-literals
		   *supported-join-operators* filter-string)))
      (if (not op-pos)
	  filter-string
	  (let* ((op-str (subseq filter-string op-pos (+ 2 op-pos)))
		 (left-str (subseq filter-string 0 op-pos))
		 (right-str (subseq filter-string (+ (length op-str) op-pos)))
		 (left-scope (find-or-and-left-scope left-str))
		 (right-scope (find-or-and-right-scope right-str))
		 (modified-str
		  (concatenate 'string (subseq left-str 0 (- (length left-str)
							     (length left-scope)))
			       "(" (if (string= op-str "||") "or" "and") " "
			       "(progn " left-scope ")" "(progn " right-scope ")) "
			       (subseq right-str (length right-scope)))))
	    (when (or (= (length (trim-whitespace left-scope)) 0)
		      (= (length (trim-whitespace right-scope)) 0))
	      (error (make-condition
		      'sparql-parser-error
		      :message (format nil "Invalid filter: \"~a\", expect an RDF term after and before: \"~a\"" original-filter-string op-str))))
	    (set-or-and-operators construct modified-str original-filter-string))))))


(defun find-binary-op-string (filter-string idx)
  "Returns the operator as string that is placed on the position idx."
  (let* ((2-ops
	  (remove-null (map 'list #'(lambda(op-string)
				      (when (= (length op-string) 2)
					op-string))
			    (*supported-binary-operators*))))
	 (operator-str (subseq filter-string idx)))
    (if (string-starts-with-one-of operator-str 2-ops)
	(subseq operator-str 0 2)
	(subseq operator-str 0 1))))


(defun find-or-and-left-scope (left-string)
  "Returns the string that is the left part of the binary scope."
  (declare (String left-string))
  (let* ((first-bracket
	  (let ((inner-value (search-first-unclosed-paranthesis left-string)))
	    (when inner-value 
	      (+ inner-value (1+ (length (name-after-paranthesis
					  (subseq left-string inner-value))))))))
	 (start-idx (if first-bracket
			first-bracket
			0)))
    (subseq left-string start-idx)))


(defun name-after-paranthesis (str)
  "Returns the substring that is contained after the paranthesis.
   str must start with a ( otherwise the returnvalue is nil."
  (declare (String str))
  (let ((result "")
	(non-whitespace-found nil))
  (when (string-starts-with str "(")
    (let ((cleaned-str (subseq str 1)))
      (dotimes (idx (length cleaned-str))
	(let ((current-char (subseq cleaned-str idx (1+ idx))))
	  (cond ((string-starts-with-one-of current-char (list "(" ")"))
		 (setf idx (length cleaned-str)))
		((and non-whitespace-found
		      (white-space-p current-char))
		 (setf idx (length cleaned-str)))
		((white-space-p current-char)
		 (push-string current-char result))
		(t
		 (push-string current-char result)
		 (setf non-whitespace-found t)))))
      result))))


(defun find-or-and-right-scope (right-string)
  "Returns the string that is the right part of the binary scope."
  (declare (String right-string))
  (let* ((first-pos (search-first-ignore-literals
		     *supported-join-operators* right-string))
	 (first-bracket
	  (let ((inner-value (search-first-unopened-paranthesis right-string)))
	    (when inner-value (1+ inner-value))))
	 (paranthesis-pair-idx
	  (let* ((cleaned-str (trim-whitespace-left right-string))
		 (bracket-scope (bracket-scope cleaned-str)))
	    (when bracket-scope
	      (+ (- (length right-string) (length cleaned-str))
		 (length bracket-scope)))))
	 (end-idx
	  (cond ((and first-pos first-bracket)
		 (if (< first-pos first-bracket)
		     (if paranthesis-pair-idx
			 (if (< first-pos paranthesis-pair-idx)
			     paranthesis-pair-idx
			     first-pos)
			 first-pos)
		     first-bracket))
		(first-bracket first-bracket)
		(first-pos
		 (if paranthesis-pair-idx
		     (if (< first-pos paranthesis-pair-idx)
			 paranthesis-pair-idx
			 first-pos)
		     first-pos))
		(t
		 (if (= (length right-string) 0)
		     0
		     (length right-string))))))
    (subseq right-string 0 end-idx)))


(defgeneric set-unary-operators (construct filter-string)
  (:documentation "Transforms the unary operators !, +, - to (not ),
                   (one+ ) and (one- ). The return value is a modified filter
                   string.")
  (:method ((construct SPARQL-Query) (filter-string String))
    (let ((result-string ""))
      (dotimes (idx (length filter-string))
	(let ((current-char (subseq filter-string idx (1+ idx))))
	  (cond ((string= current-char "!")
		 (if (and (< idx (1- (length filter-string)))
			  (string= (subseq filter-string (1+ idx) (+ 2 idx)) "="))
		     (push-string current-char result-string)
		     (let ((result (unary-operator-scope filter-string idx)))
		       (push-string "(not " result-string)
		       (push-string (set-unary-operators construct (getf result :scope))
				    result-string)
		       (push-string ")" result-string)
		       (setf idx (- (1- (length filter-string))
				    (length (getf result :next-query)))))))
		((or (string= current-char "-")
		     (string= current-char "+"))
		 (let ((string-before
			(trim-whitespace-right (subseq filter-string 0 idx))))
		   (if (or (string= string-before "")
			   (string-ends-with string-before "(progn")
			   (string-ends-with-one-of
			    string-before (append (*supported-operators*) (list "("))))
		       (let ((result (unary-operator-scope filter-string idx)))
			 (push-string (concatenate 'string "(one" current-char " ")
				      result-string)
			 (push-string (set-unary-operators construct
							   (getf result :scope))
				      result-string)
			 (push-string ")" result-string)
			 (setf idx (- (1- (length filter-string))
				      (length (getf result :next-query)))))
		       (push-string current-char result-string))))
		((or (string= current-char "'")
		     (string= current-char "\""))
		 (let ((literal
			(get-literal (subseq filter-string idx))))
		   (if literal
		       (progn
			 (setf idx (- (1- (length filter-string))
				      (length (getf literal :next-string))))
			 (push-string (getf literal :literal) result-string))
		       (push-string current-char result-string))))
		(t
		 (push-string current-char result-string)))))
      result-string)))


(defun unary-operator-scope (filter-string idx)
  "Returns a list of the form (:next-query <string> :scope <string>).
   scope contains the statement that is in the scope of one of the following
   operators !, +, -."
  (declare (String filter-string)
	   (Integer idx))
  (let* ((string-after (subseq filter-string (1+ idx)))
	 (cleaned-str (cut-comment string-after)))
    (cond ((string-starts-with cleaned-str "(")
	   (let ((result (bracket-scope cleaned-str)))
	     (list :next-query (string-after cleaned-str result)
		   :scope result)))
	  ((or (string-starts-with cleaned-str "?")
	       (string-starts-with cleaned-str "$"))
	   (let ((result (get-filter-variable cleaned-str)))
	     (list :next-query (string-after cleaned-str result)
		   :scope result)))
	  ((string-starts-with cleaned-str "\"")
	   (let ((result (get-literal cleaned-str :quotation "\"")))
	     (list :next-query (getf result :next-string)
		   :scope (getf result :literal))))
	  ((string-starts-with-digit cleaned-str)
	   (let ((result (separate-leading-digits cleaned-str)))
	     (list :next-query (string-after cleaned-str result)
		   :scope result)))
	  ((string-starts-with cleaned-str "true")
	   (list :next-query (string-after cleaned-str "true")
		 :scope "true"))
	  ((string-starts-with cleaned-str "false")
	   (list :next-query (string-after cleaned-str "false")
		 :scope "false"))
	  ((let ((pos (search-first *supported-functions* cleaned-str)))
	     (when pos
	       (= pos 0)))
	   (let ((result (function-scope cleaned-str)))
	     (list :next-query (string-after cleaned-str result)
		   :scope result)))
	  (t
	   (error
	    (make-condition
	     'sparql-parser-error
	     :message
	     (format
	      nil "Invalid filter: \"~a\". An unary operator must be followed by ~a"
	      filter-string
	      "a number, boolean, string, function or a variable")))))))


(defun function-scope (str)
  "If str starts with a supported function there is given the entire substr
   that is the scope of the function, i.e. the function name and all its
   variable including the closing )."
  (declare (String str))
  (let* ((cleaned-str (cut-comment str))
	 (after-fun
	  (remove-null (map 'list #'(lambda(fun)
				      (when (string-starts-with cleaned-str fun)
					(string-after str fun)))
			    *supported-functions*)))
	 (fun-suffix (when after-fun
		       (cut-comment (first after-fun)))))
    (when fun-suffix
      (let* ((args (bracket-scope fun-suffix))
	     (fun-name (string-until cleaned-str args)))
	(concatenate 'string fun-name args)))))


(defun get-filter-variable (str)
  "Returns the substring of str if str starts with ? or $ until the variable ends,
   otherwise the return value is nil."
  (declare (String str))
  (when (or (string-starts-with str "?")
	    (string-starts-with str "$"))
    (let ((found-end (search-first (append (white-space) (*supported-operators*)
					   *supported-brackets* (list "?" "$"))
				   (subseq str 1))))
      (if found-end
	  (subseq str 0 (1+ found-end))
	  str))))


(defun reverse-bracket-scope (str &key (open-bracket "(") (close-bracket ")"))
  "If str ends with close-bracket there will be returned the substring until
   the matching open-bracket is found. Otherwise the return value is nil."
  (declare (String str open-bracket close-bracket))
  (when (string-ends-with str close-bracket)
    (let ((local-str (subseq str 0 (1- (length str))))
	  (result ")")
	  (close-brackets 1))
      (do ((idx (1- (length local-str)))) ((< idx 0))
	(let ((current-char (subseq local-str idx (1+ idx))))
	  (push-string current-char result)
	  (cond ((string= current-char open-bracket)
		 (when (not (in-literal-string-p local-str idx))
		   (decf close-brackets))
		 (when (= close-brackets 0)
		   (setf idx 0)))
		((string= current-char close-bracket)
		 (when (not (in-literal-string-p local-str idx))
		   (incf close-brackets)))))
	(decf idx))
      (reverse result))))


(defun bracket-scope (str &key (open-bracket "(") (close-bracket ")"))
  "If str starts with open-bracket there will be returned the substring until
   the matching close-bracket is found. Otherwise the return value is nil."
  (declare (String str open-bracket close-bracket))
  (when (string-starts-with str open-bracket)
    (let ((open-brackets 0)
	  (result ""))
      (dotimes (idx (length str))
	(let ((current-char (subseq str idx (1+ idx))))
	  (cond ((or (string= "'" current-char)
		     (string= "\"" current-char))
		 (let ((literal (get-literal (subseq str idx))))
		   (if literal
		       (progn
			 (setf idx (- (1- (length str))
				      (length (getf literal :next-string))))
			 (push-string (getf literal :literal) result))
		       (progn
			 (setf result nil)
			 (setf idx (length str))))))
		((string= current-char close-bracket)
		 (decf open-brackets)
		 (push-string current-char result)
		 (when (= open-brackets 0)
		   (setf idx (length str))))
		((string= current-char open-bracket)
		 (incf open-brackets)
		 (push-string current-char result))
		(t
		 (push-string current-char result)))))
      result)))


(defgeneric set-boundings (construct query-string)
  (:documentation "Returns a list of the form (:next-query <string>
                   :filter-string <string>). next-query is a string containing
                   the query after the filter and filter is a string
                   containing the actual filter. Additionally all free
                   '(' are transformed into '(progn' and all ', ''', \"\"\"
                   are transformed into \".")
  (:method ((construct SPARQL-Query) (query-string String))
    (let ((filter-string "")
	  (open-brackets 0)
	  (result nil))
      (dotimes (idx (length query-string))
	(let ((current-char (subseq query-string idx (1+ idx))))
	  (cond ((string= "(" current-char)
		 (setf open-brackets (1+ open-brackets))
		 (if (progn-p query-string idx)
		     (push-string "(progn " filter-string)
		     (push-string current-char filter-string)))
		((string= ")" current-char)
		 (setf open-brackets (1- open-brackets))
		 (when (< open-brackets 0)
		   (error
		    (make-sparql-parser-condition
		     (subseq query-string idx)
		     (original-query construct)
		     "an opening bracket \"(\" is missing for the current closing one")))
		 (push-string current-char filter-string))
		((or (string= "'" current-char)
		     (string= "\"" current-char))
		 (let ((result
			(get-literal (subseq query-string idx) :quotation "\"")))
		   (unless result
		     (error (make-sparql-parser-condition
			     (subseq query-string idx)
			     (original-query construct)
			     "a closing character for the given literal")))
		   (setf idx (- (1- (length query-string))
				(length (getf result :next-string))))
		   (push-string (getf result :literal) filter-string)))
		((string= "#" current-char)
		 (let ((comment-string
			(string-until (subseq query-string idx)
				      (string #\newline))))
		   (setf idx (+ idx (length comment-string)))))
		((and (string= current-char (string #\newline))
		      (= 0 open-brackets))
		 (setf result
		       (list :next-query (subseq query-string idx)
			     :filter-string filter-string))
		 (setf idx (1- (length query-string))))
		((string= current-char "}")
		 (when (/= open-brackets 0)
		   (error (make-sparql-parser-condition
			   (subseq query-string idx)
			   (original-query construct)
			   (format nil
				   "a valid filter, but the filter is not complete, ~a"
				   (if (> open-brackets 0)
				       (format nil "~a ')' is missing"
					       open-brackets)
				       (format nil "~a '(' is missing"
					       open-brackets))))))
		 (setf result
		       (list :next-query (subseq query-string idx)
			     :filter-string filter-string)))
		(t
		 (push-string current-char filter-string)))))
      result)))


(defun progn-p(query-string idx)
  "Returns t if the ( at position idx in the filter string
  represents a (progn) block."
  (declare (String query-string)
	   (Integer idx))
  (let* ((delimiters (append (list " " "," (string #\Space) (string #\Tab)
				   (string #\Newline) (string #\cr) "(" ")")
			     (*supported-operators*)))
	 (string-before (trim-whitespace-right (subseq query-string 0 idx)))
	 (fragment-before-idx
	  (search-first delimiters string-before :from-end t))
	 (fragment-before
	  (if (and (not fragment-before-idx)
		   (and (> (length string-before) 0)
			(not (string-ends-with-one-of
			      (trim-whitespace-right string-before)
			      *supported-functions*))))
	      (error (make-condition
		      'SPARQL-PARSER-ERROR
		      :message (format nil "Invalid filter: \"~a\"~%"
				       query-string)))
	      (if fragment-before-idx
		  (subseq string-before fragment-before-idx)
		  nil))))
    (when fragment-before
      (mapcan #'(lambda(operator)
		  (when (and (string-starts-with fragment-before operator)
			     (> (length fragment-before) (length operator)))
		    (setf fragment-before
			  (string-after fragment-before operator))))
	      (append (*supported-operators*) *supported-brackets*)))
    (if fragment-before
	(progn
	  (when (or (string-starts-with fragment-before "?")
		    (string-starts-with fragment-before "$"))
	    (error
	     (make-condition
	      'SPARQL-PARSER-ERROR
	      :message (format nil "Invalid filter: found \"~a\" but expected ~a"
			       fragment-before *supported-functions*))))
	  (when (not (string-starts-with-one-of
		      fragment-before (append *supported-functions* delimiters)))
	    (error
	     (make-condition
	      'SPARQL-PARSER-ERROR
	      :message
	      (format nil "Invalid character: \"~a\", expected characters: ~a"
		      fragment-before (append *supported-functions* delimiters)))))
	  (if (string-ends-with-one-of fragment-before *supported-functions*)
	      nil
	      t))
	(if (find string-before *supported-functions* :test #'string=)
	    nil
	    t))))