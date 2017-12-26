#!/bin/sh
set -x

cat > normfile << EOF
5 s
EOF

ls *SAC >> normfile

gfortran norm.f90 -o norm
./norm

