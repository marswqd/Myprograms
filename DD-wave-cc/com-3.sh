#检查三分量sac文件是否都存在
#!bin/bash

cd 2013
#sacZ=(`ls ?[!.]*.*.Z.SAC`);nsacZ=${#sacZ[@]}
#sacN=(`ls ?[!.]*.*.N.SAC`);nsacN=${#sacN[@]}
#sacE=(`ls ?[!.]*.*.E.SAC`);nsacE=${#sacE[@]}
sacZ=(`ls P.*.*.Z.SAC`);nsacZ=${#sacZ[@]}
sacN=(`ls P.*.*.N.SAC`);nsacN=${#sacN[@]}
sacE=(`ls P.*.*.E.SAC`);nsacE=${#sacE[@]}
#sacPN=(`ls P.*.N.SAC`);nsacPN=${#sacPN[@]}
#sacPE=(`ls P.*.E.SAC`);nsacPE=${#sacPE[@]}
echo "Z files:" $nsacZ
echo "N files:" $nsacN
echo "E files:" $nsacE
#echo "PN files:" $nsacPN
#echo "PE files:" $nsacPE
read -n 1 -p "Press any key to continue..."
echo ----------------------------

mkdir del
for((i=0;i<nsacZ;i++));do
 Z=${sacZ[i]}
 N=${Z/.Z./.N.}
 E=${Z/.Z./.E.}
 if [ -f "$N" -a -f "$E" ];then 
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
 if [ -f "$Z" -a -f "$E" ];then 
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
 if [ -f "$Z" -a -f "$N" ];then 
  #echo $E OK
  continue
 else
  mv  $E del/
  echo $E del
 fi 
done

cd ..
