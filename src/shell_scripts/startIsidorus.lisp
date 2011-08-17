(setf sb-impl::*default-external-format* :utf-8)
(asdf:operate 'asdf:load-op 'isidorus)
;;(xtm-importer:import-from-xtm "/home/isidorus/.sbcl/site/isidorus/src/unit_tests/textgrid_07.15.2011.xtm" "data_base" :tm-id "http::/isidor.us/tm-1")
(setf rest-interface:*users* (list (list :uname "isidorus" :passwd "Ar8g7Pw")))
(setf rest-interface:*use-http-authentication* 1) ;only the host page isidorus.html is protected, note: all RESTful handlers can be invoked without any authentication mechanism!!!    
(setf rest-interface:*local-backup-remote-address* "143.93.190.247")
(setf rest-interface:*remote-backup-remote-address* "143.93.190.247")
(setf rest-interface:*shutdown-remote-address* "127.0.0.1")
(rest-interface:start-json-engine "/home/isidorus/data_base" :host-name "143.93.190.176" :port 7000)
(rest-interface:start-admin-server)
(format t "will enter die-when-finished")
(rest-interface:die-when-finished)

