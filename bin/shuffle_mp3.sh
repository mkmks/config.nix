#!/bin/sh

FILES=`ls ./*.mp3
DATE=`date``
SEED="$FILES$DATE"

for i in ./*.mp3
do
    SEED=`echo $SEED | sha256sum | cut -f 1 -d ' '`
    mv "$i" $SEED.mp3
done
    
