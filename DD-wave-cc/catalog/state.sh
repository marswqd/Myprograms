#!/bin/sh

filein=log_jg.txt
file=eve_jg.txt

awk '{if($1=="#") print $0}' ${filein} > $file
awk '{if($11<1) {a++;}} END {print "[0,1)", a}' $file
awk '{if($11>=1 && $11<2) {a++;}} END {print "[1,2)", a}' $file
awk '{if($11>=2 && $11<3) {a++;}} END {print "[2,3)", a}' $file
awk '{if($11>=3 && $11<4) {a++;}} END {print "[3,4)", a}' $file
awk '{if($11>=4 && $11<5) {a++;}} END {print "[4,5)", a}' $file
awk '{if($11>=5 && $11<6) {a++;}} END {print "[5,6)", a}' $file
awk '{if($11>=6 && $11<7) {a++;}} END {print "[6,7)", a}' $file

awk 'BEGIN {max=-10} {if($11>max) max=$11 fi} END {print "Maxmag=", max}' $file
awk 'BEGIN {min=10} {if($11<min) min=$11 fi} END {print "Minmag=", min}' $file
awk 'BEGIN {max=-200} {if($8>max) max=$8 fi} END {print "Maxlat=", max}' $file
awk 'BEGIN {min=200} {if($8<min) min=$8 fi} END {print "Minlat=", min}' $file
awk 'BEGIN {max=-200} {if($9>max) max=$9 fi} END {print "Maxlon=", max}' $file
awk 'BEGIN {min=200} {if($9<min) min=$9 fi} END {print "Minlon=", min}' $file
awk 'BEGIN {max=-200} {if($10>max) max=$10 fi} END {print "Maxdep=", max}' $file
awk 'BEGIN {min=200} {if($10<min) min=$10 fi} END {print "Mindep=", min}' $file

awk '{if($11>=6) print $0}' $file

# [0,1) 
# [1,2) 2677
# [2,3) 573
# [3,4) 125
# [4,5) 10
# [5,6) 
# [6,7) 1
# Maxmag= 6.2
# Minmag= 1.0
# Maxlat= 23.791
# Minlat= 23.081
# Maxlon= 100.703
# Minlon= 100.066
# Maxdep= 9.0
# Mindep= 1.0
#	2014	10	7	21	49	39.700	 23.374	100.465	19.0	6.2	0	0	0	1421426	云南景谷




