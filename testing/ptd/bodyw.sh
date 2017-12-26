#!/bin/sh
set -x
HS=10
#this depth and model gives a spectra hole at 33-40 sec
STK=0
RAKE=0
DIP=45
AZ=45
NMODE=5
######
# changing the RAKE to 45 removes some of the spectral hole
# =0 Strike slip if dip is 90
# =90 dip slip is dip is 45 -- good hole
######
cat > dfile << EOF
200.0 0.250 1024 -1.0 10.0
250.0 0.250 1024 -1.0 10.0
300.0 0.250 1024 -1.0 10.0
350.0 0.250 1024 -1.0 10.0
400.0 0.250 1024 -1.0 10.0
440.0 0.250 1024 -1.0 10.0
EOF
cat > model.d << EOF
MODEL.01
CUS Model with Q from simple gamma values
ISOTROPIC
KGS
FLAT EARTH
1-D
CONSTANT VELOCITY
LINE08
LINE09
LINE10
LINE11
H(KM) VP(KM/S) VS(KM/S) RHO(GM/CC) QP QS ETAP ETAS FREFP FREFS
 1.0000 5.0000 2.8900 2.5000 0.172E-02 0.387E-02 0.00 0.00 1.00 1.00
 9.0000 6.1000 3.5200 2.7300 0.160E-02 0.363E-02 0.00 0.00 1.00 1.00
10.0000 6.4000 3.7000 2.8200 0.149E-02 0.336E-02 0.00 0.00 1.00 1.00
20.0000 6.7000 3.8700 2.9020 0.000E-04 0.000E-04 0.00 0.00 1.00 1.00
 0.0000 8.1500 4.7000 3.3640 0.194E-02 0.431E-02 0.00 0.00 1.00 1.00
EOF
#####
# Chapter 3
#####
hprep96 -M model.d  -HS ${HS} \
-HR 0 -d dfile -ALL
hspec96 > hspec96.out
hpulse96 -D -p -1 4 > file96
fmech96 -A ${AZ} -ROT -D ${DIP} -R ${RAKE} \
-S ${STK} -M0 1.0E+20 < file96 > 3.96
f96tosac -B 3.96
cp B00101Z00.sac Z1.sac
cp B00201Z00.sac Z2.sac
cp B00301Z00.sac Z3.sac
cp B00401Z00.sac Z4.sac
cp B00501Z00.sac Z5.sac
cp B00601Z00.sac Z6.sac
cp B00102R00.sac R1.sac
cp B00202R00.sac R2.sac
cp B00302R00.sac R3.sac
cp B00402R00.sac R4.sac
cp B00502R00.sac R5.sac
cp B00602R00.sac R6.sac
cp B00103T00.sac T1.sac
cp B00203T00.sac T2.sac
cp B00303T00.sac T3.sac
cp B00403T00.sac T4.sac
cp B00503T00.sac T5.sac
cp B00603T00.sac T6.sac
rm B*sac

