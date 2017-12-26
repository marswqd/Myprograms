#!/bin/sh
set -x

cat > nbcfile << EOF
1 20.0 3.5
4.0 50.0 1.0
200.0 200.0
A101.A608.A.SAC

EOF

#gfortran nbc.f90 libcalpltf-ubuntu.a -o nbc
gfortran nbc.f90 libcalpltf-cyg.a -o nbc
./nbc
ls *.C.plt
#plotxvig < Tt.plt
#plotnps -G < *.plt > *.ps

