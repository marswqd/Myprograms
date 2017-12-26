#!bin/sh
#按发震时刻挑选相应的evt文件


mkdir evtfile
event=eventall.txt
year=( `awk '{printf ("%04d ",$1)}' $event` );
 mon=( `awk '{printf ("%02d ",$2)}' $event` );
 day=( `awk '{printf ("%02d ",$3)}' $event` );
hour=( `awk '{printf ("%02d ",$4)}' $event` );
 min=( `awk '{printf ("%02d ",$5)}' $event` );
n=${#min[@]};echo $n
# echo 2012 9 7 11 20 | awk '{printf ("%04d%02d%02d%02d %02d%2.0f",$1,$2,$3,$4,$5,$6)}' #=20120907 1120
evt=( `ls *.ext` );nevt=${#evt[@]}

for ((i=0;i<n;i++));do 
 eve=${year[i]}${mon[i]}${day[i]}" "${hour[i]}${min[i]}  #eve="20120901 0101"
 #echo $eve
 t=`date -d "$eve" '+%Y-%m-%d %H:%M'`  #转化为时间戳记2012-09-01 01:01
 echo "event O-time: " $t
 t0=`date -d "$t" '+%s'`               #转化为秒,相对于UTC时间1970-01-01
 t1=`expr $t0 - 3600`                  #发震时刻前后1小时
 t2=`expr $t0 + 3600`
 for ((j=0;j<nevt;j++));do
  a=${evt[j]}
  b=${a:0:8}" "${a:8:4}
  c=`date -d "$b" '+%Y-%m-%d %H:%M'`
  d=`date -d "$c" '+%s'`
  if [ $d -lt $t2 ] && [ $d -gt $t1 ];then
   echo $a
   cp $a evtfile/
  fi
 done
 echo "  "
done
