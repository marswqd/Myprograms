#!/bin/sh
set -x
# alpha 为线性时间分辨（朱良保），若两个alpha相等，给个周期的alpha就是恒定值。（mfu1.in的第三行）
num=1   #要处理的文件数
flag=0  #flag=1 截取信号窗数据进行滤波
T1=10.0
T2=100.0
dT=1.0  #起始周期；结束周期；周期间隔
alpha1=25.0
alpha2=25.0  #起始周期和结束周期对应高斯滤波参数


cat > mfu1.in << EOF
${num} ${flag} 
${T1} ${T2} ${dT} 
${alpha1} ${alpha2}
12.sac.-

EOF

gfortran mfu1.f90 -o mfu1
./mfu1


