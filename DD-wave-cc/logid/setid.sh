#!bin/sh
#set -x

catalog=my2013.txt
pgm=setid

sac=(`ls P.*.*.Z.SAC`);nsac=${#sac[@]}
echo ${catalog} > ${pgm}.in
echo ${nsac} >> ${pgm}.in
for ((m=0;m<nsac;m=m+1));do
 echo ${sac[m]} >> ${pgm}.in
done

gfortran ${pgm}.f90 -o ${pgm}
./${pgm}

#mv *.SAC ../





