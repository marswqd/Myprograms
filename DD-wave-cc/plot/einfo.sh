#!/bin/sh

#�������Ŀ¼��̨վ��Ϣ�ļ�
phafile=yl.txt
stafile=stationall.txt

gfortran einfo.f90 -o einfo
./einfo $phafile $stafile




