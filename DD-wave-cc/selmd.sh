#!bin/sh
#set -x
#挑选震中距小于maxdist且震级大于minmag的地震事件文件

minmag=1
maxdist=260
mkdir wqd2

cd ssac
sacZ=(`ls ?[!.]*.*.Z.SAC`);nsacZ=${#sacZ[@]}
sacN=(`ls ?[!.]*.*.N.SAC`);nsacN=${#sacN[@]}
sacE=(`ls ?[!.]*.*.E.SAC`);nsacE=${#sacE[@]}
echo "Z files:" $nsacZ
echo "N files:" $nsacN 
echo "E files:" $nsacE
read -n 1 -p "Press any key to continue..." 
echo ----------------------------
# sacZ[0]=P.ZAT.20121006174635.Z.SAC
# nsacZ=1
for ((i=0;i<nsacZ;i=i+1));do
 Z=${sacZ[i]}
 N=${Z/.Z./.N.}
 E=${Z/.Z./.E.}
 dist=`saclhdr -DIST ${Z}`
 mag=`saclhdr -MAG ${Z}`
 #KT0=`saclhdr -KT0 ${Z}`
 flag=a
# echo a ${Z} ${N} ${E} ${dist} ${kn}
 flag=`echo $mag $minmag | awk '{if($1>=$2) {print "b"} else {print "a"}}'`
 flag=`echo $dist $maxdist | awk '{if($1<=$2) {print "b"} else {print "a"}}'` 
 #echo $flag
 if [[ ${flag} = b ]];then
  echo ${Z} ${N} ${E} ${mag} ${dist}
  cp ${Z} ${N} ${E} ../wqd2/
 fi  
done

 
 
 





