#!bin/sh
#set -x

MOD=yl.mod
phase=2012yl.txt
ttfile=theroytt.txt
#MOD=tak135sph.mod
rm $ttfile
cat ${phase} | while read line
do
 echo $line
 flag=`echo $line | awk -F ' ' '{print $1}'`
 if [ "$flag" == "#" ];then
  evdp=`echo $line | awk -F ' ' '{print $10}'`
  echo $evdp
  echo $line >> theroytt.txt
 else
  sta=`echo $line | awk -F ' ' '{print $1}'`
  w=`echo $line | awk -F ' ' '{print $3}'`
  p=`echo $line | awk -F ' ' '{print $4}'`
  e=`echo $line | awk -F ' ' '{print $5}'`
  dist=`echo $line | awk -F ' ' '{print $6}'`
  az=`echo $line | awk -F ' ' '{print $7}'`
  if [ ${p} = P ];then
   tt=`time96 -DIST ${dist} -EVDP ${evdp} -P -T -M ${MOD}`
  else
   tt=`time96 -DIST ${dist} -EVDP ${evdp} -SV -T -M ${MOD}`
  fi
  echo $sta $tt $w $p $e $dist $az >> $ttfile
 fi
done









