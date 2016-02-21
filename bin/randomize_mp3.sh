#!/bin/sh

for i in ./*.mp3
do
    mv "$i" `echo "${i%.mp3}" | sha256sum | cut -f 1 -d ' '`.mp3
done
    
