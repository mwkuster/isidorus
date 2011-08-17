(setf sb-impl::*default-external-format* :utf-8)
(asdf:operate 'asdf:load-op 'isidorus)
;;(xtm-importer:import-from-xtm "/home/isidorus/.sbcl/site/isidorus/src/unit_tests/textgrid_07.15.2011.xtm" "data_base" :tm-id "http::/isidor.us/tm-1")
(setf rest-interface:*users* (list (list :uname "isidorus" :passwd "Ar8g7Pw")))
(setf rest-interface:*use-http-authentication* 2) ;only the host page isidorus.html and the commit handlers are protected  
(setf rest-interface:*local-backup-remote-address* "12.34.56.78") ;the allowed other remove peer ip
(setf rest-interface:*remote-backup-remote-address* "12.34.56.78") ;the allowed other remove peer ip
(setf rest-interface:*shutdown-remote-address* "127.0.0.1") ;the allowed other remove peer ip
(rest-interface:start-json-engine "/home/isidorus/data_base" :host-name "12.34.56,78" :port 12345)
(rest-interface:start-admin-server)
(format t "will enter die-when-finished")
(rest-interface:die-when-finished)

