#!/bin/bash
set -x
#######################################
## 此脚本的目的在于自动进入和退出各个台站文件夹，从各个台站文件夹中选出我们需要时间段
## 内的sac文件名和总数。每个台站响应时间段内的所有sac文件依次保存在各个台站文件夹里的
## slectfile文件里
######################################
## 作者：陈浩朋
## 创立时间：2010.12.10
######################################

directory=(`ls -d A* K* L*`) #注意这里括号里的一定是`,而非‘
ldirectory=${#directory[@]}
## directory是我们选择的台站名字，ldirectory表示的是directory里元素的个数，也即台站个数
mkdir work
rm number #number是我们存放所有台站相应时间段内sac文件个数的文件。为防止多次运行，每次都写入，开始时要删除原来可能有的文件

for ((m=0;m<ldirectory;m=m+1))
do
 mkdir work/${directory[m]}
 #cp days.sh work/days.sh
 cd ${directory[m]} #进入第m个台站目录，注意m是从0开始的
  length=94 #选择的时间天数
  len=0     #统计sac文件个数的变量
  for ((i=151;i <= length+151-1;i=i+1))  
  do
   files=(`ls *2007$i*.sac`)  #选择2007年151天到244天共94天（5月31日到9月1日）的时间范围
   len2=${#files[@]}   #len2为第i天的文件数。
   len=$((len+len2))
   for ((j=0;j<=len2-1;j=j+1))
   do
	cp ${files[j]} ../work/${directory[m]}/${files[j]}
   done
  done
  cd ..
  echo "${directory[m]}  $len">>number
done

