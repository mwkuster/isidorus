#!/bin/bash
##+-----------------------------------------------------------------------------
##+  Isidorus
##+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
##+
##+  Isidorus is freely distributable under the LLGPL license.
##+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
##+  trunk/docs/LGPL-LICENSE.txt.
##+-----------------------------------------------------------------------------


## This script can be used to invoke hunchentoot's admin interface and shut down
## the server. The default ip address is set to 127.0.0.1 and can be changed via
## the switch -host <ip-address>. The default url that determines the server's
## callback binding is set to /admin/shutdown, but this behavior can also be
## changed by using the switch -url <url-fragment>.
## a sample call would be ./shutdown-isidorus.sh -host 12.34.56.78 -url /admin/shutdown

url="/admin/shutdown";
host="127.0.0.1:11008";

if [ $# -eq 0 ]; then
    :
elif [ $# -eq 1 -a $1 = "?" ]; then
    echo "you can pass the arguments -host <host-url> and -url </url-fragment>, if no arguments are passed the default values 127.0.0.1:11008 and /admin/shutdown are used";
    exit;
elif [ $# -eq 2 ]; then
    if [ $1 = "-host" ]; then
	host=$2;
    elif [ $1 = "-url" ]; then
	url=$2;
    else
	echo "only the arguments -host and -url are supported, use ? for more information";
	exit;
    fi
elif [ $# -eq 4 ]; then
    if [ $1 = "-host" ]; then
	host=$2;
    elif [ $1 = "-url" ]; then
	url=$2;
    else
	echo "only the arguments -host and -url are supported, use ? for more information";
	exit;
    fi
    
    if [ $3 = "-host" ]; then
	host=$4;
    elif [ $3 = "-url" ]; then
	url=$4;
    else
	echo "only the arguments -host and -url are supported, use ? for more information";
	exit;
    fi
else
    echo "only the arguments -host and -url are supported, use ? for more information";
    exit;
fi
	    
curl $host$url



