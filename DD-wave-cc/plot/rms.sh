#!bin/bash

awk '{if($8>0)  { sum+=$8;n+=1}}END{print "avdx",sum/n}' hypoDD.reloc
awk '{if($9>0)  { sum+=$9;n+=1}}END{print "avdy",sum/n}' hypoDD.reloc
awk '{if($10>0) {sum+=$10;n+=1}}END{print "avdz",sum/n}' hypoDD.reloc
awk '{if($22>0) {sum+=$22;n+=1}}END{print "avrcc",sum/n}' hypoDD.reloc
awk '{if($23>0) {sum+=$23;n+=1}}END{print "avrct",sum/n}' hypoDD.reloc

