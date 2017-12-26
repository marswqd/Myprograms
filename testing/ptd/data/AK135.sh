#!/bin/sh
set -x

echo $0 $1 $2 $3 $4 $5
HS=$2
STK=244
RAKE=-84
DIP=40
AZ=$3
Mw=$4
NMODE=10
######
# 生成震中距文件和速度模型文件
######
#采样从发震时刻开始
cat > dfile << EOF
$1 1.0 2048 0.0 0.0
EOF
#AK135模型，分层深度到660公里
cat > model.d << EOF
MODEL.01
AK135 MODEL
ISOTROPIC
KGS
FLAT EARTH
1-D
CONSTANT VELOCITY
LINE08
LINE09
LINE10
LINE11
H(KM) VP(KM/S) VS(KM/S) RHO(GM/CC) QP    QS   ETAP ETAS  FREFP FREFS
20.00  5.8000   3.4600    2.7200  767.0 500.0  0.0  0.0   1.0   1.0
15.00  6.5000   3.8500    2.9200  767.0 500.0  0.0  0.0   1.0   1.0
42.50  8.0400   4.4800    3.3198  262.0 141.0  0.0  0.0   1.0   1.0
42.50  8.0450   4.4900    3.3455  262.0 141.0  0.0  0.0   1.0   1.0
45.00  8.0500   4.5000    3.3713  262.0 141.0  0.0  0.0   1.0   1.0
45.00  8.1750   4.5090    3.3985  262.0 141.0  0.0  0.0   1.0   1.0
50.00  8.3000   4.5230    3.4258  262.0 141.0  0.0  0.0   1.0   1.0
50.00  8.4825   4.6090    3.4561  262.0 141.0  0.0  0.0   1.0   1.0
50.00  8.6650   4.6960    3.4864  262.0 141.0  0.0  0.0   1.0   1.0
50.00  8.8475   4.7830    3.5167  256.0 111.0  0.0  0.0   1.0   1.0
50.00  9.3600   5.0800    3.7557  256.0 111.0  0.0  0.0   1.0   1.0
50.00  9.5280   5.1860    3.8175  296.0 134.0  0.0  0.0   1.0   1.0
50.00  9.6960   5.2920    3.8793  296.0 134.0  0.0  0.0   1.0   1.0
50.00  9.8640   5.3980    3.9410  296.0 134.0  0.0  0.0   1.0   1.0
50.00 10.0320   5.5040    4.0028  840.0 569.0  0.0  0.0   1.0   1.0
00.00 10.7900   5.9600    4.3714  840.0 569.0  0.0  0.0   1.0   1.0
EOF
#####
# Chapter 2
#####
# 生成频散信息
sprep96 -M model.d -NMOD ${NMODE} -HS ${HS} -HR 0 -d dfile -L -R
sdisp96
# 计算群/相速度频散曲线和非弹性衰减系数
sregn96
slegn96
#sdpegn96 -R -U -PER -ASC  -YMIN 2 -YMAX 6 -XMIN 2 -XMAX 500
#sdpegn96 -L -U -PER -ASC  -YMIN 2 -YMAX 6 -XMIN 2 -XMAX 500
#sdpegn96 -R -C -PER -ASC  -YMIN 2 -YMAX 6 -XMIN 2 -XMAX 500
#sdpegn96 -L -C -PER -ASC  -YMIN 2 -YMAX 6 -XMIN 2 -XMAX 500
#mv SREGNC.PLT ${5}.RC.PLT
#mv SREGNU.PLT ${5}.RU.PLT
#mv SREGN.ASC ${5}.R.ASC
#rm -f SLEGNC.PLT SLEGNU.PLT
#sdpegn96 -R -G -PER -ASC -XLOG -YMIN 2.5 -YMAX 5
#sdpegn96 -L -G -PER -ASC -XLOG -YMIN 2.5 -YMAX 5
# 画图
#plotxvig < SREGNC.PLT
#plotxvig < SLEGNC.PLT
#plotxvig < SREGNU.PLT
#plotxvig < SLEGNU.PLT
#plotxvig < SREGNG.PLT
#plotxvig < SLEGNG.PLT

#spulse96 -d dfile -D -p -EQ -2 > file96
#fmech96 -A ${AZ} -ROT -D ${DIP} -R ${RAKE} \
#-S ${STK} -MW ${Mw} < file96 > 3.96
#f96tosac -B 3.96
#cp B00101Z00.sac ${5}.SAC

spulse96 -d dfile -D -p -EQ -2 -M 0 > file96
fmech96 -A ${AZ} -ROT -D ${DIP} -R ${RAKE} \
-S ${STK} -MW ${Mw} < file96 > 3.96.0
f96tosac -B 3.96.0
cp B00101Z00.sac ${5}.0.SAC

rm -f B*.sac




