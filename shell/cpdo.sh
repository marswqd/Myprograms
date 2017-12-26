#!/bin/bash
set -x

directory=(`ls -d A* K* L*`) #注意这里括号里的一定是`,而非‘
ldirectory=${#directory[@]}
mkdir huabei2007
for ((m=0;m<ldirectory;m=m+1));do
 cp days.sh ${directory[m]}/
 cp days.f90 ${directory[m]}/
 cd ${directory[m]}
 rm *SAC
 bash days.sh
 ./days
 cp *SAC ../huabei2007/
 cd ..
done
