#!/run/current-system/sw/bin/bash

for a in *.flac; do
    TITLE=`metaflac "$a" --show-tag=TITLE | sed s/.*=//g`
    DISCNUMBER=`metaflac "$a" --show-tag=DISCNUMBER | sed s/.*=//g`
    TRACKNUMBER=`metaflac "$a" --show-tag=TRACKNUMBER | sed s/.*=//g`
    mv "$a" "$DISCNUMBER`printf %02g $TRACKNUMBER` - $TITLE.flac"
    done
