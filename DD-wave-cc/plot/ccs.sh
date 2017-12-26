#show the selected and not selected cross-correlation travel time difference data
#!bin/sh
#set -x

file1=cc-o.txt # not selected data
file2=cc-s.txt # selected data

J=X4i/4i
R=0/10/0/20
vp=5.1         # reference P wave velocity
vs=2.8         # reference S wave velocity

# awk 'BEGIN {max=-20} {if($3>max) max=$3 fi} END {print "Dist Max=", max}' $file1
# awk 'BEGIN {min=20}  {if($3<min) min=$3 fi} END {print "Dist Min=", min}' $file1

# awk 'BEGIN {max=0} {if($6>max) max=$6 fi} END {print "Dist Max=", max}' $file2
# awk 'BEGIN {min=20} {if($6<min) min=$6 fi} END {print "Dist Min=", min}' $file2

awk '{if($4=="P") print $3,sqrt($6*$6)}' $file1 | psxy -J$J -R$R -Ggray -Sc0.01i \
-B2:"Distance difference(km)":/2:"CC dt(s)"::."P-wave":WSne -K -N > ccdt.ps
y=`echo $vp | awk '{print 10/$1+0.5}'`
psxy -J$J -R -W0.01i,black -B -O -K << ! >> ccdt.ps
0.0 0.5
10 $y
!
awk '{if($4=="S") print $3,sqrt($6*$6)}' $file1 | psxy -J$J -X5.5i -R$R -Ggray -Sc0.01i \
-B2:"Distance difference(km)":/2:"CC dt(s)"::."S-wave":WSne -K -O -N >> ccdt.ps
y=`echo $vs | awk '{print 10/$1+0.5}'`
psxy -J$J -R -W0.01i,black -B -O -K << ! >> ccdt.ps
0.0 0.5
10 $y
!

awk '{if($4=="P") print $7}' $file1 | pshistogram -J$J -Xa1.5i -G100/100/100 -L1.0,white -W0.1 -R-2/2/0/210000 \
-B0.5:"Cross-correlation correction(s)":/20000:"Number"::."P-wave":WSne -V -K > ccc.ps
awk '{if($4=="P") print $7}' $file2 | pshistogram -J$J -Xa1.5i -Gblack -L1.0,white -W0.1 -R-2/2/0/210000 \
-B0.5:"Cross-correlation correction(s)":/20000:"Number"::."P-wave":WSne -V -K -O >> ccc.ps
awk '{if($4=="S") print $7}' $file1 | pshistogram -J$J -Xa7i -G100/100/100 -L1.0,white -W0.1 -R-2/2/0/210000 \
-B0.5:"Cross-correlation correction(s)":/20000:"Number"::."S-wave":WSne -V -K -O >> ccc.ps
awk '{if($4=="S") print $7}' $file2 | pshistogram -J$J -Xa7i -Gblack -L1.0,white -W0.1 -R-2/2/0/210000 \
-B0.5:"Cross-correlation correction(s)":/20000:"Number"::."S-wave":WSne -V -K -O >> ccc.ps








