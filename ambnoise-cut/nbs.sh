#!bin/bash
set -x

cat > nbsfile << EOF
6 4
7.0 10.0 15.0 20.0
A101.A608.-.SAC
A101.A608.+.SAC   
A608.L236.-.SAC
A608.L236.+.SAC 
A101.L236.-.SAC
A101.L236.+.SAC 



EOF

gfortran nbs.f90 -o nbs
./nbs

cat > nbs.s << EOF
xlim 0 200
ylim -1 1
r A101.A608.-.SAC*S
p1
EOF
sac nbs.s

rm nbsfile nbs.s
	  

