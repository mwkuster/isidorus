#!/bin/bash
##+-----------------------------------------------------------------------------
##+  Isidorus
##+  (c) 2008-2010 Marc Kuester, Christoph Ludwig, Lukas Georgieff
##+
##+  Isidorus is freely distributable under the LLGPL license.
##+  You can find a detailed description in trunk/docs/LLGPL-LICENSE.txt and
##+  trunk/docs/LGPL-LICENSE.txt.
##+-----------------------------------------------------------------------------


## This script can be used to invoke hunchentoot's admin interface and backup
## the server date. The default ip address is set to 127.0.0.1 and can be changed via
## the switch -host <ip-address>. The default url that determines the server's
## callback binding is set to /admin/backup, but this behavior can also be
## changed by using the switch -url <url-fragment>. The path variable
## contains the name of the stored xtm backup file that is created, the default
## value is a date string of the form dd.mm.yyyy:hh:mm:ss.xtm" and can be
## changed by using the switch -path <any-string>.
## A sample call would be
## ./local-backup-isidorus.sh -host 12.34.56.78 -url /admin/shutdown -path=backup.xtm

url="/admin/local-backup";
host="127.0.0.1:11008";
path=`date +"%d.%m.%y:%H:%M:%S"`".xtm"

if [ $# -eq 0 ]; then
    :
elif [ $# -eq 1 -a $1 = "?" ]; then
    echo "you can pass the arguments -host <host-url>, -url </url-fragment> and -path <any-string>, if no arguments are passed the default values 127.0.0.1:11008, /admin/backup and <current-data>.xtm are used";
    exit;
elif [ $# -eq 2 ]; then
    if [ $1 = "-host" ]; then
	host=$2;
    elif [ $1 = "-url" ]; then
	url=$2;
    elif [ $1 = "-path" ]; then
	path=$2;
    else
	echo "only the arguments -host, -url and -path are supported, use ? for more information";
	exit;
    fi
elif [ $# -eq 4 ]; then
    if [ $1 = "-host" ]; then
	host=$2;
    elif [ $1 = "-url" ]; then
	url=$2;
    elif [ $1 = "-path" ]; then
	path=$2;
    else
	echo "only the arguments -host, -url and path are supported, use ? for more information";
	exit;
    fi
    
    if [ $3 = "-host" ]; then
	host=$4;
    elif [ $3 = "-url" ]; then
	url=$4;
    elif [ $3 = "-path" ]; then
	path=$4;
    else
	echo "only the arguments -host, -url and path are supported, use ? for more information";
	exit;
    fi
elif [ $# -eq 6 ]; then
    if [ $1 = "-host" ]; then
	host=$2;
    elif [ $1 = "-url" ]; then
	url=$2;
    elif [ $1 = "-path" ]; then
	path=$2;
    else
	echo "only the arguments -host, -url and path are supported, use ? for more information";
	exit;
    fi
    
    if [ $3 = "-host" ]; then
	host=$4;
    elif [ $3 = "-url" ]; then
	url=$4;
    elif [ $3 = "-path" ]; then
	path=$4;
    else
	echo "only the arguments -host, -url and path are supported, use ? for more information";
	exit;
    fi
    if [ $5 = "-host" ]; then
	host=$6;
    elif [ $5 = "-url" ]; then
	url=$6;
    elif [ $5 = "-path" ]; then
	path=$6;
    else
	echo "only the arguments -host, -url and path are supported, use ? for more information";
	exit;
    fi
else
    echo "only the arguments -host, -url and path are supported, use ? for more information";
    exit;
fi




curl $host$url"?path="$path;