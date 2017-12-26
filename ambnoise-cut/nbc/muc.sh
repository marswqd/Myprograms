#!/bin/sh
set -x

cat > mucfile << EOF
9 25.0 20.0 3.5
4.0 30.0 1.0
ST.A101.A608.+.SAC
ST.A101.L236.+.SAC
ST.A608.L236.+.SAC
ST.A101.A608.-.SAC
ST.A101.L236.-.SAC
ST.A608.L236.-.SAC
ST.A101.A608.A.SAC
ST.A101.L236.A.SAC
ST.A608.L236.A.SAC
EOF

#gfortran nbc.f90 libcalpltf-ubuntu.a -o nbc
gfortran muc.f90 libcalpltf-cyg.a -o muc
./muc
ls *.U.plt
#plotxvig < Tt.plt
#plotnps -G < *.plt > *.ps

