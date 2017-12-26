#!bin/bash
#set -x

filect=dt.ct
catalog=yl.txt

gfortran ctme.f90 -o ctme
./ctme ${filect} ${catalog}







