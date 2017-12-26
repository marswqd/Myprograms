#!bin/sh

mon=( 01 02 03 04 05 06 07 08 09 10 11 12 )

for((i=0;i<=11;i++));do
 #echo ${mon[i]}
 mkdir ${mon[i]}
 cd ${mon[i]}
 mkdir evt kun sac
 cp ../doevt.sh evt/
 cp ../wh.sh sac/
 cd ..
done









