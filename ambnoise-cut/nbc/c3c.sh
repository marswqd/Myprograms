#!/bin/sh
set -x

cat > c3cfile << EOF
ST.K003.K030.A.SAC.C.txt
ST.K030.K051.A.SAC.C.txt
ST.K003.K051.A.SAC.C.txt
3
K003K030K051.-.3C.txt K003K030K051.-
K003K030K051.+.3C.txt K003K030K051.+
K003K030K051.A.3C.txt K003K030K051.A


EOF

#gfortran nbc.f90 libcalpltf-ubuntu.a -o nbc
gfortran c3c.f90 -o c3c
./c3c


