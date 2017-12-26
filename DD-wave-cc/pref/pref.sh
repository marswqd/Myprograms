#sac宏shell脚本文件。功能：数据预处理-去线性趋势、去均值、带通滤波和去仪器响应
#!bin/sh
#set -x
#去线性趋势、去均值、带通滤波和去仪器响应
# cat > trans.sm << EOF
# \$keys in file f1 f2 f3 f4
# r \$in
# rtr
# rmean
# taper
# *transfer from evalresp to none freqlimits \$f1 \$f2 \$f3 \$f4
# transfer from FAP s FAP/\$file to none freqlimits \$f1 \$f2 \$f3 \$f4
# *int
# *mul 1.0e9
# wh
# w over
# q
# EOF
# #仅作去线性趋势、去均值和带通滤波
# cat > pref.sm << EOF
# \$keys in f1 f2 f3 f4
# r \$in
# rtr
# rmean
# taper
# *transfer from evalresp to none freqlimits \$f1 \$f2 \$f3 \$f4
# transfer from none to none freqlimits \$f1 \$f2 \$f3 \$f4
# *int
# *mul 1.0e9
# wh
# w over
# q
# EOF
#滤波参数  由地震仪的频带范围决定
f1=0.01
f2=0.0125
f3=40
f4=50
#目前仅有这47个台站的地震仪系统振幅-相位频率响应(文件位于文件夹FAP中)
sfap=( BAS CAY CUX DAY DOC ERY FUN GEJ GOS GYA HEQ HLT HUP JIG JIH JIP JIS KMI LAC LIC LIJ LOP LUQ 
       LUS MAL MAS MEL MIL MLA MLP QIJ SIM TOH TUS WAD WES XUW YAJ YIM YOD YOS YUJ YUL YUM YUX ZAT ZOD )
lsfap=${#sfap[@]}
echo The number of stations which have response file : $lsfap
################
sac=(`ls *.SAC`)
nsac=${#sac[@]}
echo The number of sac files : $nsac
j=0
for ((m=0;m<nsac;m=m+1));do
 i=0
 a=${sac[m]}
 echo ${a}
 sta=${a:0:3}
 for ((n=0;n<lsfap;n=n+1));do
  if [ ${sta} = ${sfap[n]} ];then
   com=${a:19:1}
   fapfile="${sta}.${com}.fap"
sac << EOF
   r ${a}
   rtr
   rmean
   taper
   *transfer from evalresp to none freqlimits $f1 $f2 $f3 $f4
   transfer from FAP s FAP/${fapfile} to none freqlimits $f1 $f2 $f3 $f4
   *int
   *mul 1.0e9
   w over
   q   
EOF
   #sac trans.sm in ${a} file ${fapfile} f1 ${f1} f2 ${f2} f3 ${f3} f4 ${f4}
   i=1
   j=$((j+1))
   break
  fi
 done
 if [ ${i} -eq 0 ];then
sac << EOF
  r ${a}
  rtr
  rmean
  taper
  *transfer from evalresp to none freqlimits $f1 $f2 $f3 $f4
  transfer from none to none freqlimits $f1 $f2 $f3 $f4
  *int
  *mul 1.0e9
  w over
  q
EOF
  #sac pref.sm in ${a} f1 ${f1} f2 ${f2} f3 ${f3} f4 ${f4}
 fi
done
echo The number of files which are removed the instrument response : $j  #去仪器响应的文件数

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






