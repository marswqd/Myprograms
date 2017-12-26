#!bin/bash

num=100
datafile=dt.cc
errfile=gserr.txt

pgm=addecc

cat > ${pgm}.in << EOF
${num}
${datafile}
${errfile}
EOF


gfortran ${pgm}.f90 -o ${pgm}
./${pgm}


