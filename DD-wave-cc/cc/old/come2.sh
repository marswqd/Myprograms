#!bin/bash
#set -x

f0=0;f1=1.0;f2=2.0;f3=5.0;f4=6.0  #f0=1:带通滤波(Hz)
iphase=0               #iphase=1 P only ; iphase=2 S only ; iphase=else P and S
pwl=-0.5;pwr=0.5       #P时窗设定,震相到时左右偏移量(s);时窗内数据点最好接近2的幂,便于快速傅里叶变换(若有)
swl=-1.0;swr=1.0       #S时窗设定,震相到时左右偏移量(s);时窗内数据点最好接近2的幂,便于快速傅里叶变换(若有)
sld1=1.0;sld2=1.5      #滑动量
maxdist=510.0;maxsep=10.0;ccut=0.5  #台站到震源最大距离;最大震源分离距离;相关系数截止值;
sac=(`ls P.*.Z.SAC`);nsac=${#sac[@]}

# sacZ=(`ls P.*.Z.SAC`);lsacZ=${#sacZ[@]}
# sacR=(`ls P.*.R.SAC`);lsacR=${#sacR[@]}
# sacT=(`ls P.*.T.SAC`);lsacT=${#sacT[@]}
# echo "Z files:" $lsacZ
# echo "R files:" $lsacR 
# echo "T files:" $lsacT
# read -n 1 -p "Press any key to continue..." 
# echo ----------------------------

cat > come2.in << EOF
${f0} ${f1} ${f2} ${f3} ${f4}
${iphase} ${pwl} ${pwr} ${swl} ${swr}
${sld1} ${sld2}
${maxdist} ${maxsep} ${ccut}
${nsac}
EOF
for ((m=0;m<nsac;m=m+1));do
 echo ${sac[m]} >> come2.in
done

gfortran come2.1.f90 -o come2.1
./come2.1 come2.in






