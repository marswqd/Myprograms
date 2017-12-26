#show the selected and not selected cross-correlation travel time difference data
#!bin/sh
#set -x

# gfortran azn.f90 -o azn; ./azn;

file1=cc-s.txt # selected data
file2=azn.txt
outps=ccs.ps

J=X4i/4i
R=0/10/0/20

# awk 'BEGIN {max=-20} {if($3>max) max=$3 fi} END {print "Dist Max=", max}' $file1
# awk 'BEGIN {min=20}  {if($3<min) min=$3 fi} END {print "Dist Min=", min}' $file1

# awk 'BEGIN {max=0} {if($6>max) max=$6 fi} END {print "Dist Max=", max}' $file2
# awk 'BEGIN {min=20} {if($6<min) min=$6 fi} END {print "Dist Min=", min}' $file2



awk '{print $2,$1}' $file2 | psrose -A45 -S2in -G0 -R0/1/0/360 -X2.5i -B0.2g0.2/45g45 -F -P -K > ccs.ps
awk '{print $3,$1}' $file2 | psrose -A45 -S2in -G0 -R0/1/0/360 -Y5.5i -B0.2g0.2/45g45 -F -P -K -O >> ccs.ps






