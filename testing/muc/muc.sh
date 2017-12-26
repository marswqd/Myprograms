#!/bin/sh
set -x

cat > mucfile << EOF
1 50.0 40.0 3.9
4.0 100 1.0
c23.00


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
