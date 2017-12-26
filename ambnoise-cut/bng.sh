#!/bin/sh
set -x

cat > bngfile << EOF
30.0 3.5
K014.K049.3s
1.0 50.0 1.0
EOF

gfortran bng.f90 libcalpltf.a -o bng
./bng
plotxvig < test.plt
plotnps -G < test.plt > test.ps
#gfortran xgxs.f90 libcalpltf.a -o xgxs
#./xgxs