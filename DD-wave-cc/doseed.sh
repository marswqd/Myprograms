#!bin/sh
#��ѹ�����¼�seed�����ļ�

#seed=(`ls *.seed`)
mkdir done
for i in *.seed;do
 rdseed -pdf ${i} && mv ${i} done/ || echo "error:" ${i}
done
mkdir ../pz/
mv SAC_PZs_* ../pz/

# bash wh.sh
# perl wh.pl
