#!bin/bash
set -x

cat > ssfile << EOF
3
+ n A001.A209.2007.D.SAC
- n A001.A209.2007.D.SAC
a n A001.A209.2007.D.SAC
EOF

gfortran ss.f90 -o ss
./ss



