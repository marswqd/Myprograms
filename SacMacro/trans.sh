#!bin/sh
set -x

cat > pref.sm << EOF
******
* ��ΪSAC�꣬���ܶ�ʵ�ʵ������ݽ���Ԥ��������ȥ�������ơ�ȥ��ֵ��ȥ������Ӧ
* inΪ�����ļ���fileΪ������Ӧ�ļ���f1 f2 f3 f4 Ϊȥ������Ӧʱ���˲�����
* ע�⣺evalresp��rdseed�õ�
******
\$keys in out file f1 f2 f3 f4
\$default f1 0.003
\$default f2 0.005
\$default f3 0.3
\$default f4 0.4
r \$in

rtr
rmean
taper
*transfer from evalresp to none freqlimits \$f1 \$f2 \$f3 \$f4
transfer from FAP s FAP/\$file to none freqlimits \$f1 \$f2 \$f3 \$f4
int
*mul 1.0e9

w \$out
q

EOF

# sac=(`ls *SAC`)
# lsac=${#sac[@]}
# for ((m=0;m<lsac;m=m+1))
# do
 # a=${sac[m]}
 # b="P.${sta}"
 # sta=${a:4:3}
 # com=${a:21:1}
 # fapfile="${sta}.${com}.fap"
 # sac pref.sm in ${a} out ${b} file ${fapfile} f1 0.1 f2 1 f3 25 f4 30
# done




