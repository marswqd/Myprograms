#!/bin/sh
set -x

#���о� ��Դ��� ��λ�� ���� ���� ������ ��� ����
#sh AK135.sh $dist $z $az $Mw $STK $RAKE $DIP $name   

sh test.sh 500.0 10.0 45.0 4.0 0.0 45.0 45.0 Z1
sh test.sh 520.0 10.0 45.0 4.0 0.0 45.0 45.0 Z2

#sh test.sh 550.0 15.0 50.0 3.0 220.0 -60.0 60.0 Z2

cat > ptd.in << EOF
1
Z1.SAC
Z2.SAC

EOF

















