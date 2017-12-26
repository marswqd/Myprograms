#!bin/sh
#set -x
mkdir del

#带通滤波参数(Hz)
f1=1.0e0
f2=2.0e0
f3=7.0e0
f4=8.0e0

sac=(`ls ?[!.]*.*.?.SAC`)
nsac=${#sac[@]}
echo ${nsac} ${f1} ${f2} ${f3} ${f4} > bpf.in
for((m=0;m<nsac;m=m+1));do
 echo ${sac[m]} >> bpf.in
done

gfortran bpf.f90 -o bpf
./bpf

mv P.*.?.SAC ../../mdpb/





