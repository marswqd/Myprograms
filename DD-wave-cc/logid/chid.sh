#!bin/bash
cd 2011

SAC_DISPLAY_COPYRIGHT=0
file=(`ls P.*.*.Z.SAC`);nfile=${#file[@]}
for((i=0;i<nfile;i++));do
 Z=${file[i]}
 R=${Z/.Z./.R.}
 T=${Z/.Z./.T.}
 N=${Z/.Z./.N.}
 E=${Z/.Z./.E.}
 #info=(`saclst nxsize f ${Z}`)
 #id=${info[1]}
 id=`saclhdr -NXSIZE ${Z}`
 echo $Z $id
 #continue
 if [ "$id" -lt "1100000" ];then
  newid=$(($id+1100000))
  echo $Z $id $newid
  continue
sac << EOF
r $Z $R $T $N $E
ch nxsize $newid
wh
q
EOF
 fi
done
cd ..





