#!bin/bash
#set -x

epair=../epair09-13.txt                 #�����¼����ļ�:   # 1 2 dist12
flist=../zf09-13.txt                 #�洢Ҫ����Ĳ����ļ���
f0=0;f1=1.0;f2=2.0;f3=5.0;f4=6.0  #f0=1:��ͨ�˲�(Hz)
iphase=0               #iphase=1 P only ; iphase=2 S only ; iphase=else P and S
pwl=-0.5;pwr=1.0       #Pʱ���趨,���ൽʱ����ƫ����(s);ʱ�������ݵ���ýӽ�2����,���ڿ��ٸ���Ҷ�任(����)
swl=-0.5;swr=1.0       #Sʱ���趨,���ൽʱ����ƫ����(s);ʱ�������ݵ���ýӽ�2����,���ڿ��ٸ���Ҷ�任(����)
slidewin=0             #slidewin=2 ˫������ ; slidewin=else ��������
sld1=1.5;sld2=2.0      #������
maxdist=250.0;maxsep=10.0;ccut=0.5  #̨վ����Դ������;�����Դ�������;���ϵ����ֵֹ;
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






