#!/bin/sh

#输入地震目录和台站信息文件
phafile=yl.txt
stafile=stationall.txt

gfortran einfo.f90 -o einfo
./einfo $phafile $stafile




