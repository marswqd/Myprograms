#!/bin/bash

#year,day:选为基准时间的年,日;用于不同文件数据点对应时刻的统一计算
year=2007
day=001
nday=365      #想要得到的sac文件包含的天数

#将符合条件的文件的文件名组成数组files,时间顺序为由早及晚
#注意:其顺序必须为由早及晚,此地方易出错,须检验
files=(`ls *sac`)
length=${#files[@]}

#生成控制文件,供days.f90使用
cat > daysfile << EOF
$year $day $nday 
$length
EOF
ls *.sac >> daysfile

gfortran days.f90 -o days
#./days


