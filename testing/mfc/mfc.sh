#!/bin/sh
set -x

cat > mfc.in << EOF
4
1  60.0 4.078 
2.0 6.0
2.0 200.0 1.0
100000 3000
c12.sac
c23.sac
c34.sac
c24.sac
EOF

gfortran mfc.f90 libcalpltf-cyg.a -o mfc
./mfc
ls *.MFC.plt
#plotxvig < Tt.plt
#plotnps -G < *.plt > *.ps

#ËÑË÷µã  10.0 3.219  20.0 3.374  40.0 3.934  60.0 4.078