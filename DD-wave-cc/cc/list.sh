#!bin/bash

list=zfile.txt

cd cpsac

sac=(`ls CP.*.Z.SAC`);nsac=${#sac[@]}
echo ${nsac} > ${list}
for ((m=0;m<nsac;m=m+1));do
 echo ${sac[m]} >> ${list}
done

cp zfile.txt ../