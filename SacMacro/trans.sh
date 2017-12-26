#!bin/sh
set -x

cat > pref.sm << EOF
******
* 此为SAC宏，功能对实际地震数据进行预处理，包括去线性趋势、去均值和去仪器响应
* in为输入文件，file为仪器响应文件，f1 f2 f3 f4 为去仪器响应时的滤波参数
* 注意：evalresp由rdseed得到
******
\$keys in out file f1 f2 f3 f4
\$default f1 0.003
\$default f2 0.005
\$default f3 0.3
\$default f4 0.4
r \$in

rtr
rmean
taper
*transfer from evalresp to none freqlimits \$f1 \$f2 \$f3 \$f4
transfer from FAP s FAP/\$file to none freqlimits \$f1 \$f2 \$f3 \$f4
int
*mul 1.0e9

w \$out
q

EOF

# sac=(`ls *SAC`)
# lsac=${#sac[@]}
# for ((m=0;m<lsac;m=m+1))
# do
 # a=${sac[m]}
 # b="P.${sta}"
 # sta=${a:4:3}
 # com=${a:21:1}
 # fapfile="${sta}.${com}.fap"
 # sac pref.sm in ${a} out ${b} file ${fapfile} f1 0.1 f2 1 f3 25 f4 30
# done




