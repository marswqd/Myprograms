#!/bin/bash
set -x
#######################################
## �˽ű���Ŀ�������Զ�������˳�����̨վ�ļ��У��Ӹ���̨վ�ļ�����ѡ��������Ҫʱ���
## �ڵ�sac�ļ�����������ÿ��̨վ��Ӧʱ����ڵ�����sac�ļ����α����ڸ���̨վ�ļ������
## slectfile�ļ���
######################################
## ���ߣ��º���
## ����ʱ�䣺2010.12.10
######################################

directory=(`ls -d A* K* L*`) #ע�������������һ����`,���ǡ�
ldirectory=${#directory[@]}
## directory������ѡ���̨վ���֣�ldirectory��ʾ����directory��Ԫ�صĸ�����Ҳ��̨վ����
mkdir work
rm number #number�����Ǵ������̨վ��Ӧʱ�����sac�ļ��������ļ���Ϊ��ֹ������У�ÿ�ζ�д�룬��ʼʱҪɾ��ԭ�������е��ļ�

for ((m=0;m<ldirectory;m=m+1))
do
 mkdir work/${directory[m]}
 #cp days.sh work/days.sh
 cd ${directory[m]} #�����m��̨վĿ¼��ע��m�Ǵ�0��ʼ��
  length=94 #ѡ���ʱ������
  len=0     #ͳ��sac�ļ������ı���
  for ((i=151;i <= length+151-1;i=i+1))  
  do
   files=(`ls *2007$i*.sac`)  #ѡ��2007��151�쵽244�칲94�죨5��31�յ�9��1�գ���ʱ�䷶Χ
   len2=${#files[@]}   #len2Ϊ��i����ļ�����
   len=$((len+len2))
   for ((j=0;j<=len2-1;j=j+1))
   do
	cp ${files[j]} ../work/${directory[m]}/${files[j]}
   done
  done
  cd ..
  echo "${directory[m]}  $len">>number
done

