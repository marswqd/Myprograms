#!bin/bash
#set -x

#sac=(`ls P.*.Z.SAC`)
sac=(`ls ?[!.]*.*.Z.SAC`)
lsac=${#sac[@]}
echo ${lsac} > catdd.in
for ((m=0;m<lsac;m=m+1));do
 echo ${sac[m]} >> catdd.in
done

gfortran catdd.f90 -o catdd
./catdd







