#!/bin/sh
set -x

cat > nbcfile << EOF
1 30.0 4.0
4.0 40.0 1.0
A001.A209.2007.A.SAC
EOF

#gfortran nbc.f90 libcalpltf-ubuntu.a -o nbc
gfortran nbc.f90 libcalpltf-cyg.a -o nbc
./nbc
ls *plt
#plotxvig < Tt.plt
#plotnps -G < *.plt > *.ps

