#sac宏shell脚本文件。功能：带通滤波
#!bin/sh
#set -x
#####################

for i in *.SAC;do
sac << EOF
r $i
bp n 4 p 2 c 2 5
w P.$i
q
EOF
done

