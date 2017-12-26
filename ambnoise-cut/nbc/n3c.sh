#!/bin/sh
set -x

cat > n3cfile << EOF
3
ST.K003.K030.A.SAC.C.txt
ST.K030.K051.A.SAC.C.txt
ST.K003.K051.A.SAC.C.txt
K003K030K051.A
ST.K003.K030.+.SAC.C.txt
ST.K030.K051.+.SAC.C.txt
ST.K003.K051.+.SAC.C.txt
K003K030K051.+
ST.K003.K030.-.SAC.C.txt
ST.K030.K051.-.SAC.C.txt
ST.K003.K051.-.SAC.C.txt
K003K030K051.-


EOF

#gfortran nbc.f90 libcalpltf-ubuntu.a -o nbc
gfortran n3c.f90 -o n3c
./n3c


