#sac宏shell脚本文件。功能：数据预处理-去线性趋势、去均值、带通滤波
#!bin/sh
#set -x
#去线性趋势、去均值、带通滤波
# cat > pref.sm << EOF
# \$keys in f1 f2 f3 f4
# r \$in
# rtr
# rmean
# transfer from none to none freqlimits \$f1 \$f2 \$f3 \$f4
# wh
# w over
# q
# EOF
#滤波参数  由地震仪的频带范围决定
f1=0.01
f2=0.0125
f3=40
f4=50
################
sac=(`ls *.SAC`)
nsac=${#sac[@]}
echo The number of sac files : $nsac
j=0
for ((m=0;m<nsac;m=m+1));do
 i=0
 a=${sac[m]}
 echo ${a}
sac << EOF
 r ${a}
 rtr
 rmean
 taper
 transfer from none to none freqlimits $f1 $f2 $f3 $f4
 wh
 w over
 q
EOF
 #sac pref.sm in ${a} f1 ${f1} f2 ${f2} f3 ${f3} f4 ${f4}
done
 
