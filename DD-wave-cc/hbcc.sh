#!bin/bash


file=epcc1.txt
n=0
n1=
n2=
cd epcc1
touch $file
cat ccfile.txt | while read line;do
 n=$(($n+1))
 echo $n $line
 if [ $n -ge $n1 ];then 
  if [ -f "$line" ];then
   cat $line >> $file
  fi 
 fi
 if [ $n -ge $n2 ];then
  break
 fi
done
cp $file ../$file
cd ..



