#!bin/sh
#set -x

catalog=

sac=(`ls P.*.*.Z.SAC`);nsac=${#sac[@]}
echo ${catalog} > selid.in
echo ${nsac} >> selid.in
for ((m=0;m<nsac;m=m+1));do
 echo ${sac[m]} >> selid.in
done

gfortran selid.f90 -o selid
./selid

#mv *.SAC ../





