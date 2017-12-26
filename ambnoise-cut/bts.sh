#!bin/bash
set -x

cat > btsfile << EOF
730
bp 4.0 60.0
tn ob
sw on 50
EOF

ls A101*SAC >> btsfile
ls L236*SAC >> btsfile

gfortran bts.f90 -o bts
./bts

