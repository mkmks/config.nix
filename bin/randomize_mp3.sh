#!/bin/sh

SALT=`ls ./*.mp3 | sha256sum`

for i in ./*.mp3
do
    mv "$i" `echo "$SALT${i%.mp3}" | sha256sum | cut -f 1 -d ' '`.mp3
done
    
