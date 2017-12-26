#!bin/sh
#set -x
#MOD:CPS-mode96 format
#phase: sta tt dist az phase id slat slon depth
#out: sta tt caltt dist az phase id slat slon depth

MOD=ylZJS.mod
#MOD=tak135sph.mod
phase=tt.txt
out=ttt-ylZJS.txt

rm $out
touch $out
sta=(`awk '{print $1}' $phase`)
n=${#sta[@]};echo $nsta
tt=(`awk '{print $2}' $phase`)
dist=(`awk '{print $3}' $phase`)
az=(`awk '{print $4}' $phase`)
p=(`awk '{print $5}' $phase`)
id=(`awk '{print $6}' $phase`)
stla=(`awk '{print $7}' $phase`)
stlo=(`awk '{print $8}' $phase`)
evdp=(`awk '{print $9}' $phase`)
for((i=0;i<n;i++));do
 flag=`echo ${dist[i]} | awk '{if($1==-999.0) print "a";else print "b"}'`
 if [ $flag = a ];then
  continue
 fi
 if [ ${p[i]} = P ];then
  caltt=`time96 -DIST ${dist[i]} -EVDP ${evdp[i]} -P -T -M ${MOD}`
 else
  caltt=`time96 -DIST ${dist[i]} -EVDP ${evdp[i]} -SV -T -M ${MOD}`
 fi
 echo ${sta[i]} ${tt[i]} $caltt ${dist[i]} ${az[i]} ${p[i]} ${id[i]} ${stla[i]} ${stlo[i]} ${evdp[i]} | \
 awk '{printf "%-7s%8.3f%8.3f%8.3f%10.3f%5s%8d%8.3f%10.3f%9.3f\n",$1,$2,$3,$4,$5,$6,$7,$8,$9,$10}'
 echo ${sta[i]} ${tt[i]} $caltt ${dist[i]} ${az[i]} ${p[i]} ${id[i]} ${stla[i]} ${stlo[i]} ${evdp[i]} | \
 awk '{printf "%-7s%8.3f%8.3f%8.3f%10.3f%5s%8d%8.3f%10.3f%9.3f\n",$1,$2,$3,$4,$5,$6,$7,$8,$9,$10}' >> $out
done


