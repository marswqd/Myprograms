#!bin/bash
#set -x

evefile=eve_sel.txt
loc=(`ls *.reloc`);nloc=${#loc[@]};

echo ${evefile} > sloc.in
for((i=0;i<nloc;i++));do
 echo ${loc[i]} >> sloc.in
done

gfortran sloc.f90 -o sloc
./sloc

mkdir id
mv id_*.txt id/




