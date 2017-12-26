#!bin/sh
#按发震时刻挑选相应的evt文件

event=eventall.txt
pwd=/cygdrive/e/evtfile

mkdir ${pwd}
evt=( `ls *.ext` );nevt=${#evt[@]}
cat > getevt.in << EOF
$event
$pwd
$nevt
EOF
for ((i=0;i<nevt;i++));do 
 echo ${evt[i]} >> getevt.in
done

gfortran getevt.f90 -o getevt
./getevt


