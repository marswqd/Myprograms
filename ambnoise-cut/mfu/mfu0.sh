#!bin/bash
set -x

cat > mfu0.in << EOF
1 + 5
20.0 30.0 40.0 50.0 70.0 100.0
5.0 15.0 10.0 10.0 5.0  3.0
12.sac.-



EOF

gfortran mfu0.f90 -o mfu0
./mfu0

# cat > mfu.s << EOF
# xlim -300 300
# r A001.A209.2007.D.SAC*A
# p1
# EOF
# sac mfu.s

#rm mfufile mfu.s
	  

