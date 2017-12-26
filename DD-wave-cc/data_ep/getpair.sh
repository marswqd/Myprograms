#!bin/sh
#set -x


file=dt.ct
awk '{if($1=="#") print $0}' $file > epair.txt



