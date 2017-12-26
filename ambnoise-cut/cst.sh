#!bin/bash
set -x

echo 365 2 d > cstfile
ls BTS.K002* >> cstfile
ls BTS.K030* >> cstfile

gfortran cst.f90 -o cst
./cst
