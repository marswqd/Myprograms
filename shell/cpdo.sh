#!/bin/bash
set -x

directory=(`ls -d A* K* L*`) #ע�������������һ����`,���ǡ�
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
