#!bin/sh
#set -x
#将震中距大于maxdist且无标定的地震事件文件移动到delete文件夹中
mkdir wqd


#maxdist=560

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
 flag=`echo $dist $mag | awk '{if($2<=5 && $2>=3) {print "b"} else {print "a"}}'`
 #echo $flag
 if [[ ${flag} = b ]];then
  echo ${Z} ${N} ${E} ${mag}
  cp ${Z} ${N} ${E} wqd/
 fi  
done

 
 
 





