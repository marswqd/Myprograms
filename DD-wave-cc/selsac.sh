#!bin/bash
#挑选震级大于minmag,震中距小于maxdist的sac文件

sacZ=(`ls ?[!.]*.*.Z.SAC`);nsacZ=${#sacZ[@]}
sacN=(`ls ?[!.]*.*.N.SAC`);nsacN=${#sacN[@]}
sacE=(`ls ?[!.]*.*.E.SAC`);nsacE=${#sacE[@]}
#sacPN=(`ls P.*.N.SAC`);nsacPN=${#sacPN[@]}
#sacPE=(`ls P.*.E.SAC`);nsacPE=${#sacPE[@]}
echo "Z files:" $nsacZ
echo "N files:" $nsacN
echo "E files:" $nsacE
#echo "PN files:" $nsacPN
#echo "PE files:" $nsacPE
read -n 1 -p "Press any key to continue..."
echo ----------------------------

maxdist=250
minmag=1.0
for((i=0;i<nsacZ;i++));do
 Z=${sacZ[i]}
 N=${Z/.Z./.N.}
 E=${Z/.Z./.E.}
 #PN=P.${N}
 #PE=P.${E}
 info=(`saclst mag dist f ${Z}`)
 mag=${info[1]}
 dist=${info[2]}
 # dist=`saclhdr -DIST ${Z}`
 # KA=`saclhdr -KA ${Z}`
 # KT0=`saclhdr -KT0 ${Z}`
 # id=`saclhdr -NXSIZE ${Z}`
 #echo "$mag >= $min" | bc
 flag=`echo $mag $minmag $dist $maxdist | \
       awk '{if($1>=$2 && $3<=$4) print "a"; else print "b"}'`
 #echo $flag
 if [[ "$flag" = "a" ]];then
  cp ${Z} ${N} ${E} ../../magone/
  echo ${Z} ${N} ${E} $mag $dist
 fi
done

