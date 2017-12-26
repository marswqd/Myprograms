#!bin/bash
set -x

cat > ss.in << EOF
2
+ n 12.sac
- n 12.sac



EOF

gfortran ss.f90 -o ss
./ss


