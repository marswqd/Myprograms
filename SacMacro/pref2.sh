#sac宏shell脚本文件。功能：数据预处理-去线性趋势、去均值、带通滤波
#!bin/sh
#set -x
#去线性趋势、去均值、带通滤波
cat > pref.sm << EOF
\$keys in f1 f2 f3 f4
r \$in
rtr
rmean
transfer from none to none freqlimits \$f1 \$f2 \$f3 \$f4
wh
w over
q
EOF
#滤波参数  由地震仪的频带范围决定
f1=0.01
f2=0.0125
f3=40
f4=50
################
sac=(`ls *.SAC`)
lsac=${#sac[@]}
echo The number of sac files : $lsac
j=0
for ((m=0;m<lsac;m=m+1));do
 i=0
 a=${sac[m]}
 echo ${a}
 sac pref.sm in ${a} f1 ${f1} f2 ${f2} f3 ${f3} f4 ${f4}
done

#rm pref.sm trans.sm


# cat > pref.sm << EOF
# ******
# * 此为SAC宏，功能对实际地震数据进行预处理，包括去线性趋势、去均值和去仪器响应
# * in为输入文件，file为仪器响应文件，f1 f2 f3 f4 为去仪器响应时的滤波参数
# * 注意：evalresp由rdseed得到
# ******
# \$keys in out file f1 f2 f3 f4
# \$default f1 0.003
# \$default f2 0.005
# \$default f3 0.3
# \$default f4 0.4
# r \$in

# rtr
# rmean
# taper
# *transfer from evalresp to none freqlimits \$f1 \$f2 \$f3 \$f4
# transfer from FAP s FAP/\$file to none freqlimits \$f1 \$f2 \$f3 \$f4
# int
# *mul 1.0e9

# w \$out
# q

# EOF






