#!bin/sh
#set -x


#带通滤波参数(Hz)


sac=(`ls ?[!.]*.*.Z.SAC`)
nsac=${#sac[@]}
echo ${nsac} > addtt.in
for ((m=0;m<nsac;m=m+1));do
 echo ${sac[m]} >> addtt.in
done

gfortran addtt.f90 -o addtt
./addtt







