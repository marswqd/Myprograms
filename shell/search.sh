#!/bin/bash
set -x

directory=(`ls -d A* K* L*`) #ע�������������һ����`,���ǡ�
ldirectory=${#directory[@]}
for ((m=0;m<ldirectory;m=m+1))
do
 cd ${directory[m]}
 file=(`ls *SAC`)
 len=${#file[@]}
 if [ $len -ne 0 ];then
  cp *SAC ../all/
 fi
 cd ..
 if [ $len -ne 0 ];then
  echo ${directory[m]} >>search
 fi  
done
