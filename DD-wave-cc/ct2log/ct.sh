#!bin/sh
#set -x

sac=(`ls P.*.Z.SAC`)
lsac=${#sac[@]}
echo ${lsac} > ct.in
ls P.*.Z.SAC >> ct.in

gfortran ct.f90 -o ct
./ct







