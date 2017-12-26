#!bin/sh
#set -x

MOD=ylFLH.mod
#MOD=YN.mod
#MOD=tak135sph.mod

#比较不同速度模型,标定走时和理论走时的区别(P,S)
for i in P.*.Z.SAC;do
 dist=`saclhdr -DIST ${i}`
 evdp=`saclhdr -EVDP ${i}`
 KA=`saclhdr -KA ${i}`
 KT0=`saclhdr -KT0 ${i}`
 echo ${i}
 if [ ${KA} = P ];then
  P=`saclhdr -A ${i}`
  ttp=`time96 -DIST ${dist} -EVDP ${evdp} -P -T -M ${MOD}`  
  echo 'dist:' ${dist} 'dep:' ${evdp}  
  echo '   P:' ${P} 
  echo 'calP:' ${ttp}
  read -n 1 -p "Press any key to continue..."  
 fi
 
 if [ ${KT0} = S ];then
  S=`saclhdr -T0 ${i}`
  tts=`time96 -DIST ${dist} -EVDP ${evdp} -SV -T -M ${MOD}`
  echo 'dist:' ${dist} 'dep:' ${evdp}
  echo '    S:' ${S} 
  echo 'calSV:' ${tts}
  read -n 1 -p "Press any key to continue..."
 fi
done


# i=P.MAL.20120907115801.SHZ.SAC
# dist=`saclhdr -DIST ${i}`
# evdp=`saclhdr -EVDP ${i}`
# P=`saclhdr -A ${i}`
# S=`saclhdr -T0 ${i}`
# KA=`saclhdr -KA ${i}`
# KT0=`saclhdr -KT0 ${i}`
# ttp=`time96 -DIST ${dist} -EVDP ${evdp} -P -T -M ${MOD}`
# tts=`time96 -DIST ${dist} -EVDP ${evdp} -SV -T -M ${MOD}`
# echo ${dist} ${evdp} ${KA} ${KT0}
# echo ${P} ${S} 
# echo ${ttp} ${tts} 






