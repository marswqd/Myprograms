#sac��shell�ű��ļ������ܣ�����Ԥ����-ȥ�������ơ�ȥ��ֵ����ͨ�˲�
#!bin/sh
#set -x
#ȥ�������ơ�ȥ��ֵ����ͨ�˲�
# cat > pref.sm << EOF
# \$keys in f1 f2 f3 f4
# r \$in
# rtr
# rmean
# transfer from none to none freqlimits \$f1 \$f2 \$f3 \$f4
# wh
# w over
# q
# EOF
#�˲�����  �ɵ����ǵ�Ƶ����Χ����
f1=0.01
f2=0.0125
f3=40
f4=50
################
sac=(`ls *.SAC`)
nsac=${#sac[@]}
echo The number of sac files : $nsac
j=0
for ((m=0;m<nsac;m=m+1));do
 i=0
 a=${sac[m]}
 echo ${a}
sac << EOF
 r ${a}
 rtr
 rmean
 taper
 transfer from none to none freqlimits $f1 $f2 $f3 $f4
 wh
 w over
 q
EOF
 #sac pref.sm in ${a} f1 ${f1} f2 ${f2} f3 ${f3} f4 ${f4}
done
 
