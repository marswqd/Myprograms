#!bin/bash
set -x

cat > btsallfile << EOF
730
bp 4.0 50.0
ts 50 7.0 45.0 50
EOF

ls L204*SAC >> btsallfile
ls A302*SAC >> btsallfile

gfortran btsall.f90 -o btsall
./btsall

