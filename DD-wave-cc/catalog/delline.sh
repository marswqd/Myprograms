#!bin/bash

file=CEDC-JG.txt

# ɾ������
awk 'NF > 0' ${file} > a.txt

#ɾ���ļ��е��ظ���
#sort -k2n a.txt | uniq > b.txt
