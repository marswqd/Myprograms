#!bin/sh
#set -x


#mkdir ../seedsac/
SAC_DISPLAY_COPYRIGHT=0
j=0
for i in *.SAC;do
 j=$((j+1))
 echo $j
sac << EOF
r $i
wh
q
EOF
 #mv ${i} ../seedsac/
done






