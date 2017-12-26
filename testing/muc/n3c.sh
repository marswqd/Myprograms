#!/bin/sh
set -x

cat > n3cfile << EOF
3
ST.A101.A608.A.SAC.UC.txt
ST.A608.L236.A.SAC.UC.txt
ST.A101.L236.A.SAC.UC.txt
A101A608L236.A.U
ST.A101.A608.+.SAC.UC.txt
ST.A608.L236.+.SAC.UC.txt
ST.A101.L236.+.SAC.UC.txt
A101A608L236.+.U
ST.A101.A608.-.SAC.UC.txt
ST.A608.L236.-.SAC.UC.txt
ST.A101.L236.-.SAC.UC.txt
A101A608L236.-.U


EOF

#gfortran nbc.f90 libcalpltf-ubuntu.a -o nbc
gfortran n3c.f90 -o n3c
./n3c


