#!bin/sh
#set -x

MOD=yl.mod
#MOD=YN.mod
#MOD=tak135sph.mod

sacZ=(`ls ?[!.]*.*.Z.SAC`);nsacZ=${#sacZ[@]}
sacN=(`ls ?[!.]*.*.N.SAC`);nsacN=${#sacN[@]}
sacE=(`ls ?[!.]*.*.E.SAC`);nsacE=${#sacE[@]}
sacPN=(`ls P.*.N.SAC`);nsacPN=${#sacPN[@]}
sacPE=(`ls P.*.E.SAC`);nsacPE=${#sacPE[@]}
echo "Z files:" $nsacZ
echo "N files:" $nsacN 
echo "E files:" $nsacE
echo "PN files:" $nsacPN 
echo "PE files:" $nsacPE
read -n 1 -p "Press any key to continue..." 
echo ----------------------------


#若震相走时不存在,则赋为理论计算值(P,S)
for ((i=0;i<nsacN;i=i+1));do
 Z=${sacZ[i]}
 N=${Z/.Z./.N.}
 E=${Z/.Z./.E.}
 PN=P.${N}
 PE=P.${E}
 dist=`saclhdr -DIST ${Z}`
 evdp=`saclhdr -EVDP ${Z}`
 ttp=`saclhdr -A ${Z}`
 tts=`saclhdr -T0 ${Z}`
 KA=`saclhdr -KA ${Z}`
 KT0=`saclhdr -KT0 ${Z}`
 
 #tp=-12345.0;ts=-12345.0
 echo ${Z} ${N} ${E}
 echo ${PN} ${PE}
 echo ${ttp} ${tts}
 flagp=`echo ${ttp} ${KA} | awk '{if($1==-12345.0) print "a";else print "b"}'`
 if [ $flagp = a ];then
  ttp=`time96 -DIST ${dist} -EVDP ${evdp} -P -T -M ${MOD}`
 fi
 flags=`echo ${tts} ${KT0} | awk '{if($1==-12345.0) print "a";else print "b"}'`
 if [ $flags = a ];then
  tts=`time96 -DIST ${dist} -EVDP ${evdp} -SV -T -M ${MOD}`
 fi
 if [ ${flagp} = a -o ${flags} = a ];then
  sac << EOF
  r ${Z} ${N} ${E} ${PN} ${PE}
  ch A ${ttp} T0 ${tts}
  wh
  w over
  q
EOF
 fi
done

 
 
 





