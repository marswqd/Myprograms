#!bin/bash
set -x

cat > mfsfile << EOF
7 d 5
7.0 10.0 20.0 30.0 40.0 50.0 
5.0 15.0 10.0 10.0 5.0  3.0
A111.L219.2007.D.SAC
A111.A602.2007.D.SAC
A602.L219.2007.D.SAC
A111.A602.3-.SAC
A111.A602.3+.SAC
A602.L219.3-.SAC
A602.L219.3+.SAC



EOF

gfortran mfu.f90 -o mfu
./mfu

cat > mfs.s << EOF
xlim -300 300
r A111.L219.2007.D.SAC*A
p1
EOF
sac mfs.s

rm mfsfile mfs.s
	  

