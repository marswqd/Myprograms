#sac��shell�ű��ļ������ܣ�����Ԥ����-ȥ�������ơ�ȥ��ֵ����ͨ�˲�
#!bin/sh
#set -x
#ȥ�������ơ�ȥ��ֵ����ͨ�˲�
cat > pref.sm << EOF
\$keys in f1 f2 f3 f4
r \$in
rtr
rmean
transfer from none to none freqlimits \$f1 \$f2 \$f3 \$f4
wh
w over
q
EOF
#�˲�����  �ɵ����ǵ�Ƶ����Χ����
f1=0.01
f2=0.0125
f3=40
f4=50
################
sac=(`ls *.SAC`)
lsac=${#sac[@]}
echo The number of sac files : $lsac
j=0
for ((m=0;m<lsac;m=m+1));do
 i=0
 a=${sac[m]}
 echo ${a}
 sac pref.sm in ${a} f1 ${f1} f2 ${f2} f3 ${f3} f4 ${f4}
done

#rm pref.sm trans.sm


# cat > pref.sm << EOF
# ******
# * ��ΪSAC�꣬���ܶ�ʵ�ʵ������ݽ���Ԥ��������ȥ�������ơ�ȥ��ֵ��ȥ������Ӧ
# * inΪ�����ļ���fileΪ������Ӧ�ļ���f1 f2 f3 f4 Ϊȥ������Ӧʱ���˲�����
# * ע�⣺evalresp��rdseed�õ�
# ******
# \$keys in out file f1 f2 f3 f4
# \$default f1 0.003
# \$default f2 0.005
# \$default f3 0.3
# \$default f4 0.4
# r \$in

# rtr
# rmean
# taper
# *transfer from evalresp to none freqlimits \$f1 \$f2 \$f3 \$f4
# transfer from FAP s FAP/\$file to none freqlimits \$f1 \$f2 \$f3 \$f4
# int
# *mul 1.0e9

# w \$out
# q

# EOF






