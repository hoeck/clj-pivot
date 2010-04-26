#!/bin/sh

for file in $( find -iname '*.jar' ); do
    echo -n $file "   "
    jarsigner -verify $file;
done

