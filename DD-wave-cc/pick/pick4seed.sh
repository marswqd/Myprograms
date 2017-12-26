#!bin/sh
#set -x

pgm=pick4seed
#catalog=201401.txt
re_year=2014  #接近文件的日期,避免出现多个闰年
re_days=1     #所有sac文件和地震目录的总的参考时间
#catalog=log_jg.txt
catalog=20141206_5.8_5.9_JG-phase.txt

cp ${catalog} ${pgm}.f90 seedsac/done/
cd seedsac/done/

# sac=(`find . -maxdepth 1 -name "*.??.??[ZNE].?.SAC"`)
# nsac=${#sac[@]};echo $nsac 
# echo ${catalog} > pick.in
# echo ${nsac} ${re_year} ${re_days} >> pick.in
# for ((m=0;m<nsac;m=m+1));do
 # echo ${sac[m]#./} >> pick.in
# done

gfortran ${pgm}.f90 -o ${pgm}
./${pgm}

cp station.txt pick.log ../../

mkdir ../../sac0
mv *.[ZNE].SAC ../../sac0/





