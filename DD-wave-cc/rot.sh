#sac宏shell脚本文件。功能：利用内部命令将台站波形记录的两个水平分量(N,E)旋转大圆的径向和切向(R,T)
#!bin/sh
#set -x
#####################
# cat > rot.sm << EOF
# r \$1 \$2
# ch file 1 CMPAZ 0 KCMPNM N
# ch file 2 CMPAZ 90 KCMPNM E
# wh
# rot to gcp
# w \$3 \$4
# r \$3 \$4
# ch file 1 KCMPNM R
# ch file 2 KCMPNM T
# w over
# q
# EOF

cd 2012

sacN=(`ls P.*.N.SAC`);nsacN=${#sacN[@]}
 sac=(`ls P.*.E.SAC`); nsac=${#sac[@]}
if [ ${nsacN} -ne ${nsac} ];then
 echo The number of files of N and E are not equal, please check!
 echo "N files:" $nsacN 
 echo "E files:" $nsac
 read -n 1 -p "Press any key to continue..." 
fi
echo The number of files: $nsacN
echo ----------------------------

SAC_DISPLAY_COPYRIGHT=0
for ((m=0;m<nsacN;m=m+1));do
 a=${sacN[m]}
 #sacZ=${a/.N./.Z.}
 sacE=${a/.N./.E.}
 sacR=${a/.N./.R.}
 sacT=${a/.N./.T.}
 if [ -f "$sacR" -a -f "$sacT" ];then
   continue
 fi
 echo ${sacN[m]} ${sacE}
 echo ${sacR} ${sacT}
 info=(`saclst npts f ${sacE}`)
 nE=${info[1]}
 info=(`saclst npts f ${a}`)
 nN=${info[1]}
 #info=(`saclst npts f ${sacZ}`)
 #nZ=${info[1]}
 if [ $nN -ne $nE ];then
   min=`echo $nE $nN | awk '{if($1<$2) print $1;else print $2}'`
   echo "NPTS not equal"
sac << EOF
 cut B N $min
 r ${sacN[m]} ${sacE}
 w over
 q
EOF
 fi
sac << EOF
 r ${sacN[m]} ${sacE}
 ch file 1 CMPAZ 0 KCMPNM N
 ch file 2 CMPAZ 90 KCMPNM E
 wh
 rot to gcp
 w ${sacR} ${sacT}
 r ${sacR} ${sacT}
 ch file 1 KCMPNM R
 ch file 2 KCMPNM T
 wh
 q
EOF
 #sac rot.sm ${sacN[m]} ${sacE} ${sacR} ${sacT}
 echo ----------------------------
done

cd ..

#rm rot.sm



