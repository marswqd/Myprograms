#sac��shell�ű��ļ������ܣ���ͨ�˲�
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

