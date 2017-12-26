#检查三分量sac文件是否都存在
#!bin/bash

cd 2010
#sacZ=(`ls ?[!.]*.*.Z.SAC`);nsacZ=${#sacZ[@]}
#sacN=(`ls ?[!.]*.*.N.SAC`);nsacN=${#sacN[@]}
#sacE=(`ls ?[!.]*.*.E.SAC`);nsacE=${#sacE[@]}
sacZ=(`ls P.*.*.Z.SAC`);nsacZ=${#sacZ[@]}
sacN=(`ls P.*.*.N.SAC`);nsacN=${#sacN[@]}
sacE=(`ls P.*.*.E.SAC`);nsacE=${#sacE[@]}
sacR=(`ls P.*.*.R.SAC`);nsacR=${#sacR[@]}
sacT=(`ls P.*.*.T.SAC`);nsacT=${#sacT[@]}
#sacPN=(`ls P.*.N.SAC`);nsacPN=${#sacPN[@]}
#sacPE=(`ls P.*.E.SAC`);nsacPE=${#sacPE[@]}
echo "Z files:" $nsacZ
echo "N files:" $nsacN
echo "E files:" $nsacE
echo "R files:" $nsacR
echo "T files:" $nsacT
#echo "PN files:" $nsacPN
#echo "PE files:" $nsacPE
read -n 1 -p "Press any key to continue..."
echo ----------------------------

mkdir del
for((i=0;i<nsacZ;i++));do
 Z=${sacZ[i]}
 N=${Z/.Z./.N.}
 E=${Z/.Z./.E.}
 R=${Z/.Z./.R.}
 T=${Z/.Z./.T.}
 if [ -f "$N" -a -f "$E" -a -f "$R" -a -f "$T" ];then
  #echo $Z OK
  continue
 else
  mv  $Z del/
  echo $Z del
 fi 
done
for((i=0;i<nsacN;i++));do
 N=${sacN[i]}
 Z=${N/.N./.Z.}
 E=${N/.N./.E.}
 R=${N/.N./.R.}
 T=${N/.N./.T.}
 if [ -f "$Z" -a -f "$E" -a -f "$R" -a -f "$T" ];then
  #echo $N OK
  continue
 else
  mv  $N del/
  echo $N del
 fi 
done
for((i=0;i<nsacE;i++));do
 E=${sacE[i]}
 Z=${E/.E./.Z.}
 N=${E/.E./.N.}
 R=${E/.E./.R.}
 T=${E/.E./.T.}
 if [ -f "$Z" -a -f "$N" -a -f "$R" -a -f "$T" ];then
  #echo $E OK
  continue
 else
  mv  $E del/
  echo $E del
 fi 
done
for((i=0;i<nsacR;i++));do
 R=${sacR[i]}
 Z=${R/.R./.Z.}
 N=${R/.R./.N.}
 E=${R/.R./.E.}
 T=${R/.R./.T.}
 if [ -f "$Z" -a -f "$N" -a -f "$E" -a -f "$T" ];then
  #echo $R OK
  continue
 else
  mv  $R del/
  echo $R del
 fi
done
for((i=0;i<nsacT;i++));do
 T=${sacT[i]}
 Z=${T/.T./.Z.}
 N=${T/.T./.N.}
 E=${T/.T./.E.}
 R=${T/.T./.R.}
 if [ -f "$Z" -a -f "$N" -a -f "$E" -a -f "$R" ];then
  #echo $T OK
  continue
 else
  mv  $T del/
  echo $T del
 fi
done

cd ..
