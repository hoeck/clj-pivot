#!/bin/bash

JAR=$1
if [ -e $JAR ] && [ "$#" -eq 1 ]; then
    echo $JAR
    if [ -d /tmp/stripclj ]; then
        rm -rf /tmp/stripclj
    else
        mkdir /tmp/stripclj
    fi
    
    unzip $JAR -d /tmp/stripclj
    find /tmp/stripclj -iname '*.clj' -exec rm -v {} \;
    jar cvf $JAR -C /tmp/stripclj .
    rm -rf /tmp/stripclj
 else
     echo "syntax: strip-cljs [jarfile]"
     echo "remove all .clj files from the given jarfile."
 fi

