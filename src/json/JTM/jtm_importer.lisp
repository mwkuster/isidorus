;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------


(in-package :jtm)


(defun get-item-from-jtm-reference (reference-string &key (revision *TM-REVISION*)
				    prefixes)
  "Returns a ReifiableConstructC that is bound to the reference that is
   passed to this function. If the construct cannot be found the error
   tm-reference-error is thrown."
  (declare (Integer revision)
	   (List prefixes)
	   (String reference-string))
  (let* ((identifier-type
	  (get-identifier-type-from-jtm-reference reference-string))
	 (identifier-value (subseq reference-string 3))
	 (identifier-uri
	  (compute-uri-from-jtm-identifier identifier-value prefixes))
	 (construct
	  (d::get-item-by-identifier identifier-uri :revision revision
				     :identifier-type-symbol identifier-type)))
    (if construct
	construct
	(error (make-condition 'missing-reference-error :message (format nil "From get-item-from-jtm-reference(): cannot find the item identified by \"~a\"(~a)" identifier-uri reference-string)
			       :reference identifier-uri)))))	


(defun get-items-from-jtm-references (reference-strings &key (revision *TM-REVISion*)
				      prefixes)
  "Returns a list of ReifiableConstructCs that are referenced via the
   string-values in reference-strings."
  (declare (List reference-strings prefixes)
	   (Integer revision))
  (map 'list #'(lambda(reference-string)
		 (get-item-from-jtm-reference reference-string :revision revision
					      :prefixes prefixes))
       reference-strings))


(defun compute-uri-from-jtm-identifier (identifier-value prefixes)
  "Returns the full uri of an identifier string, i.e.
   * if the value is of the form '[pref:value]' the return value is
     the concatenation of 'value-of-pref' and 'value'.
   * if the value is of the form 'full-uri' the return value is
     'full-uri'"
  (declare (String identifier-value)
	   (List prefixes))
  (cond ((and (string-starts-with identifier-value "[")
	      (string-ends-with identifier-value "]"))
	 (let* ((pref-name
		 (let ((value (string-until identifier-value ":")))
		   (when value
		     (subseq value 1))))
		(suffix
		 (when pref-name
		   (let ((value
			  (subseq identifier-value (1+ (length pref-name)))))
		     (when value
		       (subseq value (min 1 (length value))
			       (max 0 (1- (length value)))))))))
	   (when (or (not pref-name) (not suffix))
	     (error (make-condition 'JTM-error :message (format nil "From compute-uri-from-jtm-identifier: the section within the range of \"[\" and \"]\" must be of the form prefix:suffix, but is: \"~a\"" identifier-value))))
	   (compute-full-uri prefixes pref-name suffix)))
	((> (length identifier-value) 0)
	 identifier-value)
	(t
	 (error (make-condition 'JTM-error :message (format nil "From compute-uri-from-jtm-identifier(): the identifier-value must be of the form \"[pref:value]\" or \"full-uri\", but is: \"~a\"" identifier-value))))))


(defun get-identifier-type-from-jtm-reference (identifier-string)
  "Returns the symbol 'PersistentIdC if identifier-string starts
   with si:, 'SubjectLocatorC if identifier-string starts with
   sl:, or 'ItemIdentifierC if identifier-string starts with ii:.
   If identifier-string do not start with one of these strings
   the error JTM-error is thrown."
  (cond ((string-starts-with identifier-string "ii:")
	 'ItemIdentifierC)
	((string-starts-with identifier-string "si:")
	 'PersistentIdC)
	((string-starts-with identifier-string "sl:")
	 'SubjectLocatorC)
	(t
	 (error (make-condition 'JTM-error :message (format nil "From get-identifier-type(): the identifier value must start with one of \"ii:\", \"si:\", or \"sl:\", but is: \"~a\"" identifier-string))))))