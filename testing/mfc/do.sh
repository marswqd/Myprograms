#!/bin/sh
set -x
HS=40
#this depth and model gives a spectra hole at 33-40 sec
STK=0
RAKE=0
DIP=90
AZ=45
NMODE=5
Mw=5
######
# changing the RAKE to 45 removes some of the spectral hole
# =0 Strike slip if dip is 90
# =90 dip slip is dip is 45 -- good hole
######

######
# 生成震中距文件和速度模型文件
######
#采样从发震时刻开始
cat > dfile << EOF
 500.0 1.0 2048 0.0 0.0
1000.0 1.0 2048 0.0 0.0
2000.0 1.0 2048 0.0 0.0
3000.0 1.0 2048 0.0 0.0
4000.0 1.0 2048 0.0 0.0
8000.0 1.0 2048 0.0 0.0
EOF
#AK135模型，分层深度到660公里
cat > model.d << EOF
MODEL
TEST MODEL
ISOTROPIC
KGS
FLAT EARTH
1-D
CONSTANT VELOCITY
LINE08
LINE09
LINE10
LINE11
HR VP VS RHO QP QS ETAP ETAS FREFP FREFS
40. 6.0 3.5 2.8 0.0 0.0 0.0 0.0 1.0 1.0
00. 8.0 4.7 3.3 0.0 0.0 0.0 0.0 1.0 1.0
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
sdpegn96 -R -C -PER -ASC  -YMIN 2 -YMAX 6 -XMIN 2 -XMAX 500
sdpegn96 -L -C -PER -ASC  -YMIN 2 -YMAX 6 -XMIN 2 -XMAX 500
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
cp B00101Z00.sac Z1.0.SAC
cp B00201Z00.sac Z2.0.SAC
cp B00301Z00.sac Z3.0.SAC
cp B00401Z00.sac Z4.0.SAC
cp B00501Z00.sac Z5.0.SAC
cp B00601Z00.sac Z6.0.SAC

rm -f B*.sac




