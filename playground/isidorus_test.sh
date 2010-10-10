#!/bin/bash

host="http://192.168.0.6:8000";

wDir="isidorus_test";
resDir="results"
logDir="logfiles"

T="true";
Nil="false";

doReq1=$T;
doReq2=$T;
doReq3=$T;
doReq4=$T;

dir1="req1";
dir2="req2";
dir3="req3";
dir4="req4";

req1=$host"/isidorus/json/psis/";
req2=$host"/isidorus/json/get/http://textgrid.org/serviceregistry/development/webpublish"
req3=$host"/isidorus/json/tmcl/types/"
req4=$host"/isidorus/json/topicstubs/http://textgrid.org/serviceregistry/development/webpublish"

log1=$logDir"/"$dir1"/iteration_";
log2=$logDir"/"$dir2"/iteration_";
log3=$logDir"/"$dir3"/iteration_";
log4=$logDir"/"$dir4"/iteration_";

res1=$resDir"/"$dir1"/iteration_";
res2=$resDir"/"$dir2"/iteration_";
res3=$resDir"/"$dir3"/iteration_";
res4=$resDir"/"$dir4"/iteration_";

function flow {
    echo "==== iteration: ${1} ====";
    counter=$1;
    if [ $1 -lt 10 ]; then
	counter="0000"$1;
    else
	if [ $1 -lt 100 ]; then
	    counter="000"$1;
	else
	    if [ $1 -lt 1000 ]; then
		counter="00"$1;
	    else
		if [ $1 -lt 10000 ]; then
		    counter="0"$1;
		fi
	    fi
	fi
    fi


    if [ $doReq1 == $T ]; then
	path1=$log1$counter;
	result1=$res1$counter;
	wget -o $path1".log" -O $result1".res" $req1;
    fi

    if [ $doReq2 == $T ]; then
	path2=$log2$counter;
	result2=$res2$counter;
	wget -o $path2".log" -O $result2".res" $req2;
    fi

    if [ $doReq3 == $T ]; then
	path3=$log3$counter;
	result3=$res3$counter;
	wget -o $path3".log" -O $result3".res" $req3;
    fi

    if [ $doReq4 == $T ]; then
	path4=$log4$counter;
	result4=$res4$counter;
	wget -o $path4".log" -O $result4".res" $req4;
    fi
}


function init {
    mkdir $wDir;
    cd $wDir;

    if [ $doReq1 == $T ]; then
	mkdir -p $logDir"/"$dir1;
	mkdir -p $resDir"/"$dir1;
    fi

    if [ $doReq2 == $T ]; then
	mkdir -p $logDir"/"$dir2;
	mkdir -p $resDir"/"$dir2;
    fi

    if [ $doReq3 == $T ]; then
	mkdir -p $logDir"/"$dir3;
	mkdir -p $resDir"/"$dir3;
    fi

    if [ $doReq4 == $T ]; then
	mkdir -p $logDir"/"$dir4;
	mkdir -p $resDir"/"$dir4;
    fi
}



function main {
    init;
    
    for i in `seq 1 200000`; do
	flow $i;
    done
}



main;
