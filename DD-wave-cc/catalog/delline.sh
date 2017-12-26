#!bin/bash

file=CEDC-JG.txt

# 删除空行
awk 'NF > 0' ${file} > a.txt

#删除文件中的重复行
#sort -k2n a.txt | uniq > b.txt
