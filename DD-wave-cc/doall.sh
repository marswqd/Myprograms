#!bin/sh
#循环进入文件夹进行其他操作,因需要进行修改

year=( 2009 2010 2011 2012 2013 2014 )
mon=( 01 02 03 04 05 06 07 08 09 10 11 12 )

min=1.0
for((k=3;k<=5;k++));do
 cd ${year[k]}
 mkdir magone
 for((j=0;j<=11;j++));do
  #mkdir ${year[k]}${mon[i]}
  #cd ${year[k]}${mon[i]}
  #mv kun evtsac
  #mkdir evt evtsac sac
  #cp ../doevt.sh evt/
  #cp ../wh.sh sac/
  #mv kun evtsac
  #cd evt
  #bash doevt.sh
  cd ${year[k]}${mon[j]}/sac
  for i in *.Z.SAC;do
   a=(`saclst mag f ${i}`)
   #;echo ${a[0]} ${a[1]} ${a[2]}
   mag=${a[1]}
   #echo "$mag >= $min" | bc
   flag=`echo $mag $min | awk '{if($1>=$2) print "a"; else print "b"}'`
   #echo $flag
   if [[ "$flag" = "a" ]];then
    Z=${i}
    N=${Z/.Z./.N.}
    E=${Z/.Z./.E.}
    cp ${Z} ${N} ${E} ../../magone/
    echo ${Z} ${N} ${E} $mag
   fi
  done
  cd ..
  cd ..
 done
 cd ..
done









