#!bin/sh
#��ѹ�����¼�evt�����ļ�
#����evt2sacΪ�������ļ�,ѡ��32λLinux������(Ubuntu64��װ32λ����)

#EVT=(`ls *EVT`)
mkdir done
for i in *ext��do
 evt2sac ${i} && mv ${i} done/ || echo "error:" ${i}
done

# bash wh.sh
# perl wh.pl
