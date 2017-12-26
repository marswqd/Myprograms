#!/bin/sh
set -x

cat > ngcfile << EOF
9 30.0 3.8
4.0 50.0 1.0
ST.K003.K030.+.SAC
ST.K003.K051.+.SAC
ST.K030.K051.+.SAC
ST.K003.K030.-.SAC
ST.K003.K051.-.SAC
ST.K030.K051.-.SAC
ST.K003.K030.A.SAC
ST.K003.K051.A.SAC
ST.K030.K051.A.SAC


EOF

#gfortran nbc.f90 libcalpltf-ubuntu.a -o nbc
gfortran ngc.f90 libcalpltf-cyg.a -o ngc
./ngc
ls *plt
#plotxvig < Tt.plt
#plotnps -G < *.plt > *.ps

