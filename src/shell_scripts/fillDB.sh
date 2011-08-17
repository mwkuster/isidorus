#!/bin/bash
export LANG=de_DE.UTF-8
cd /home/isidorus/.sbcl/site/isidorus/src
sbcl << END
(setf sb-impl::*default-external-format* :UTF-8)
(asdf:operate 'asdf:load-op 'isidorus)
(xtm-importer:setup-repository "/home/isidorus/textgrid.xtm" "/home/isidorus/data_base" :tm-id "http://textgrid.org/serviceregistry/services-tm")
(elephant:close-store)
END
