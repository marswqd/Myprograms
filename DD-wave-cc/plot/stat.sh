#!/bin/sh
#awk '{if($1=="#") print $0}' yl.txt > a.txt
awk '{if($11<=1) {a++;}} END {print a}' a.txt
awk '{if($11>=1 && $11<=2) {a++;}} END {print a}' a.txt
awk '{if($11>2 && $11<=3) {a++;}} END {print a}' a.txt
awk '{if($11>3 && $11<=4) {a++;}} END {print a}' a.txt
awk '{if($11>4 && $11<=5) {a++; print $0}} END {print a}' a.txt
awk '{if($11>5 && $11<=6) {a++;}} END {print a}' a.txt

awk 'BEGIN {max=0} {if($11>max) max=$11 fi} END {print "Max=", max}' a.txt
awk 'BEGIN {min=10} {if($11<min) min=$11 fi} END {print "Min=", min}' a.txt



# awk '{printf "#\t%d\t%d\t%d\t%d\t%d\t%6.3f\t%8.4f\t%8.4f\t%4.1f\t%3.1f\t0\t0\t0\t%d\n",  \
      # $3,$4,$5,$6,$7,$8,$9,$10,$11,$15,NR}' yl.txt > b.txt