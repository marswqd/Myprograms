#!bin/sh
#set -x

sac=(`ls *.*.*.??Z`)
nsac=${#sac[@]}
echo ${nsac} > getsta.in
for ((m=0;m<nsac;m=m+1));do
 echo ${sac[m]} >> getsta.in
done

gfortran getsta.f90 -o getsta
./getsta

#mv *.SAC ../





