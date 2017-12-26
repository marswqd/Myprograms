#!bin/sh
#set -x
mkdir delete

#sac=(`ls ?[!.]*.*.?.SAC`)
sac=(`ls *.SAC`)
nsac=${#sac[@]}
echo ${nsac} > delete0.in
for ((m=0;m<nsac;m=m+1));do
 echo ${sac[m]} >> delete0.in
done

gfortran delete0.f90 -o delete0
./delete0







