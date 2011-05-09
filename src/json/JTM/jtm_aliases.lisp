;;+-----------------------------------------------------------------------------
;;+  Isidorus
;;+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
;;+
;;+  Isidorus is freely distributable under the LLGPL license.
;;+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
;;+  trunk/docs/LGPL-LICENSE.txt.
;;+-----------------------------------------------------------------------------

(defpackage :jtm-exporter
  (:use :cl :json :datamodel :base-tools :isidorus-threading
	:constants :exceptions :jtm)
  (:export :import-from-jtm
	   :import-form-jtm-string
	   :export-as-jtm
	   :export-as-jtm-string
	   :export-construct-as-jtm-string
	   :*jtm-xtm*
	   :item_type-topicmap
	   :item_type-topic
	   :item_type-name
	   :item_type-variant
	   :item_type-occurrence
	   :item_type-association
	   :item_type-role))


(defpackage :jtm-importer
  (:use :cl :json :datamodel :base-tools :isidorus-threading
	:constants :exceptions :jtm)
  (:export :import-from-jtm))