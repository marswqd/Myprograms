#!bin/bash
#set -x


sac=(`ls P.*.Z.SAC`);nsac=${#sac[@]}

for ((i=0;i<nsac;i=i+1));do
 Z=${sac[i]}
 R=${Z/.Z./.R.}
 T=${Z/.Z./.T.}
 eve=${Z:11:3}
 if [ "$eve" \< "918" -a "$eve" \> "906" ];then
  echo $Z
  cp $Z $R $T ../CCSAC2
 fi
done
 
 







