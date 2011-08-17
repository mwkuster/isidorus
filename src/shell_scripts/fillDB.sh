#!/bin/bash
export LANG=de_DE.UTF-8
cd /home/isidorus/.sbcl/site/isidorus/src
su -l -s /bin/bash isidorus -c sbcl << END
(setf sb-impl::*default-external-format* :UTF-8)
(asdf:operate 'asdf:load-op 'isidorus)
(xtm-importer:setup-repository "/home/isidorus/latestDump20110711.xtm" "/home/isidorus/data_base" :tm-id "http://textgrid.org/serviceregistry/services-tm")
(elephant:close-store)
END
