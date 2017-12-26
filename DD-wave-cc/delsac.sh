#!bin/bash


file=2009md

cd ${file}
mkdir del
for Z in *.Z.SAC;do
 N=${Z/.Z./.N.}
 E=${Z/.Z./.E.}
 info=(`saclst b e a t0 dist f ${Z}`)
 b=${info[1]}
 e=${info[2]}
 p=${info[3]}
 s=${info[4]}
 dist=${info[5]}
 #echo "$mag >= $min" | bc
 flag=`echo $dist | awk '{if($1>250) print "a"; else print "b"}'` 
 if [ "$flag" = "a" ];then
  mv ${Z} ${N} ${E} del/
  echo ${Z} ${N} ${E} $b $e $p $s ${dist}
  continue
 fi 
 flag=`echo $p $s | awk '{if($1<0 && $2<0) print "a"; else print "b"}'` 
 if [ "$flag" = "a" ];then
  mv ${Z} ${N} ${E} del/
  echo ${Z} ${N} ${E} $b $e $p $s ${dist}
  continue
 fi 
 flag1=`echo $b $e $p | \
        awk '{if($3>=0 && ($3<=$1 || $3>=$2)) print "a"; else print "b"}'`
 flag2=`echo $b $e $s | \
        awk '{if($3>=0 && ($3<=$1 || $3>=$2)) print "a"; else print "b"}'`
 if [ "$flag1" = "a" -o "$flag2" = "a" ];then
  mv ${Z} ${N} ${E} del/
  echo ${Z} ${N} ${E} $b $e $p $s ${dist}
 fi
done
cd ..

