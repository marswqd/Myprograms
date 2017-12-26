#!/bin/sh
set -x

cat > nbcfile << EOF
30.0 3.5
K014.K049.3s
1.0 50.0 1.0
EOF

gfortran nbc.f90 libcalpltf.a -o nbc
./nbc
plotxvig < test.plt
plotnps -G < test.plt > test.ps
#gfortran xgxs.f90 libcalpltf.a -o xgxs
#./xgxs