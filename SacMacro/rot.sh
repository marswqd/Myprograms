#sac宏shell脚本文件。功能：利用内部命令将台站波形记录的两个水平分量(N,E)旋转大圆的径向和切向(R,T)
#!bin/sh
#set -x
#####################
cat > rot.sm << EOF
r \$1 \$2
ch file 1 CMPAZ 0 KCMPNM N
ch file 2 CMPAZ 90 KCMPNM E
wh
rot to gcp
w \$3 \$4
r \$3 \$4
ch file 1 KCMPNM R
ch file 2 KCMPNM T
w over
q
EOF

sacN=(`ls *.N.SAC`)
lsacN=${#sacN[@]}
sac=(`ls *.E.SAC`)
lsac=${#sacN[@]}
if [ ${lsacN} -ne ${lsac} ];then
 echo The number of files of N and E are not equal, please check! 
fi
echo The number of files: $lsacN
echo ----------------------------

for ((m=0;m<lsacN;m=m+1));do
 echo ${sacN[m]}
 a=${sacN[m]:0:18}
 sacE="${a}.E.SAC"
 sacR="${a}.R.SAC"
 sacT="${a}.T.SAC" 
 echo ${sacE}
 echo ${sacR}
 echo ${sacT}
 sac rot.sm ${sacN[m]} ${sacE} ${sacR} ${sacT}
 echo ----------------------------
done

#rm rot.sm



