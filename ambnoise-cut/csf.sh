#!bin/bash
set -x

echo 365 2 d > csffile
ls BRMS.L204*SAC >> csffile
ls BRMS.A302*SAC >> csffile

gfortran csf.f90 -o csf
./csf
