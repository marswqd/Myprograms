#!/bin/sh
set -x

cat > n3gcfile << EOF
1
ST.K003.K030.A.SAC.C.txt
ST.K003.K051.A.SAC.C.txt
ST.K030.K051.A.SAC.C.txt
K003K030K051.A


EOF

#gfortran nbc.f90 libcalpltf-ubuntu.a -o nbc
gfortran n3gc.f90 -o n3gc
./n3gc

