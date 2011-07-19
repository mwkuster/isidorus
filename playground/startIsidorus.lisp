;; this file must be called with sblc --load <this-file>

(setf sb-impl::*default-external-format* :utf-8)
(asdf:operate 'asdf:load-op 'isidorus)
(rest-interface:start-json-engine "data_base")
(rest-interface:start-admin-server)
(rest-interface:die-when-finished)

