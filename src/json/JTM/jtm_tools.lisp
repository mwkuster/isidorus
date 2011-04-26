;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :jtm
  (:use :cl :json :datamodel)
  (:export :jtm-import
	   :jtm-export
	   :*json-xtm*))

(in-package :jtm)

(defvar *jtm-xtm* "jtm-xtm"); Represents the currently active TM of the JTM-Importer