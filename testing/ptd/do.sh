#!/bin/sh
set -x

#震中距 震源深度 方位角 矩震级 走向 滑动角 倾角 名称
#sh AK135.sh $dist $z $az $Mw $STK $RAKE $DIP $name   

sh test.sh 500.0 10.0 45.0 4.0 0.0 45.0 45.0 Z1
sh test.sh 520.0 10.0 45.0 4.0 0.0 45.0 45.0 Z2

#sh test.sh 550.0 15.0 50.0 3.0 220.0 -60.0 60.0 Z2

cat > ptd.in << EOF
1
Z1.SAC
Z2.SAC

EOF

















