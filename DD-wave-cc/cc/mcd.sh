#!bin/bash
#set -x

epair=../epair09-13.txt                 #地震事件对文件:   # 1 2 dist12
flist=../zf09-13.txt                 #存储要处理的波形文件名
f0=0;f1=1.0;f2=2.0;f3=5.0;f4=6.0  #f0=1:带通滤波(Hz)
iphase=0               #iphase=1 P only ; iphase=2 S only ; iphase=else P and S
pwl=-0.5;pwr=1.0       #P时窗设定,震相到时左右偏移量(s);时窗内数据点最好接近2的幂,便于快速傅里叶变换(若有)
swl=-0.5;swr=1.0       #S时窗设定,震相到时左右偏移量(s);时窗内数据点最好接近2的幂,便于快速傅里叶变换(若有)
slidewin=0             #slidewin=2 双滑动窗 ; slidewin=else 单滑动窗
sld1=1.5;sld2=2.0      #滑动量
maxdist=250.0;maxsep=10.0;ccut=0.5  #台站到震源最大距离;最大震源分离距离;相关系数截止值;
pgm=mcd

cat > ${pgm}.in << EOF
${epair}
${f0} ${f1} ${f2} ${f3} ${f4}
${iphase} ${pwl} ${pwr} ${swl} ${swr}
${slidewin} ${sld1} ${sld2}
${maxdist} ${maxsep} ${ccut}
${flist}
EOF

cp ${pgm}.f90 cut09-13/${pgm}.f90
cp ${pgm}.in cut09-13/${pgm}.in
cd cut09-13
gfortran ${pgm}.f90 -o ${pgm}
./${pgm}






