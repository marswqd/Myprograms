#!/bin/bash
#���ܣ�ʹ��GMT����������-̨վ�ֲ�ͼ
#------------------------------------------------------
#�Ӵ����������н�ȡС����
#grd1=CNgtopo.grd
#grd1=YNsrtm.grd
#grd2=yl.grd
R=102/106/25.5/29.5
map=ylsta.ps


#grdcut ${grd1} -G${grd2} -R${R}
#������ɫ��
#grd2cpt ${grd2} -Crelief -T+ -Z > yl.cpt
#grd2cpt ${grd2} -Cnrwc.cpt -T= -Z > yl.cpt
#���ӱ������䣨��λ��0�ȣ�����ǿ�����
#grdgradient ${grd2} -Gyl.grad -A0 -Ne 
#grdgradient ${grd2} -Gyl.grad -A120 -M
#��������ͼ
#grdimage yl.grd -JM5.5i -R -B1/1:."Station Distribution": \
#-Cyl.cpt -Iyl.grad -K > ${map}
psbasemap -JM5.5i -R${R} -B1/1 -K > ${map}
#����ɫ�ʱ��
#psscale -D7i/3i/4i/0.3i -Cyl.cpt -I -E -B1000/:km: -K -O >> ${map}
#����ʡ��
psxy prov.txt -JM -R -W1.0p,red,- -B -K -O >> ${map}
#�����ϲ�
# psxy ynmainfault.dat -JM -R -W1.0p/blue  -B -K -O >> yiliang.ps
# #�������ϲ�ı�ʾ��
# pstext -JM -R -Gblack -B -K -O -P << ! >> yiliang.ps
# 98.685   26.657  17 0 1 2 A
# 99.933   24.896  17 0 1 2 B
# 100.727  27.069  17 0 1 2 C
# 101.602  24.221  17 0 1 2 D
# 103.224  26.501  17 0 1 2 E
# !
#�����ϲ㣨ȫ���ϲ㣬�������ڷ�Χ����ֻ����ʾ97/107/21/30�ڵĶϲ㣩
psxy ynfault.txt -JM -R -W1.0p,blue -B -K -O >> ${map}
#���������ϲ㣺��ͨ��³�����
# psxy -JM -R -W1.8p/black -B -K -O << ! >> ${map}
# 104.0999 27.78207
# 104.0708 27.74029
# 104.0195 27.69116
# 103.9678 27.6348
# 103.9118 27.58001
# 103.8603 27.53618
# 103.8008 27.47577
# 103.7505 27.43017
# 103.7127 27.39774
# 103.6732 27.36072
# 103.6341 27.32013
# 103.595 27.27862
# 103.5648 27.25641
# 103.5406 27.23541
# 103.534 27.22878
# 103.534 27.22878
# !
#��̨վ
awk '{print $3,$2}' staCTC.txt | psxy -JM -R -Gblack -St0.2i -B -K -O >> ${map}
awk '{print $3,$2+0.08,12,0,0,"CB",$1}' staCTC.txt | pstext -JM -R -Gblack -B -K -O >> ${map}
#����̨
# awk '{print $3,$2}' staqy.txt | psxy -JM -R -Gblack -St0.2i -B -K -O >> ${map}
# awk '{print $3,$2+0.1,12,0,1,"CB",$1}' staqy.txt | pstext -JM -R -Gblack -B -K -O >> ${map}
#����̨
# awk '{print $3,$2}' stals.txt | psxy -JM -R -Gblue -Si0.2i -B -K -O >> ${map}
# awk '{print $3,$2+0.1,12,0,1,"CB",$1}' stals.txt | pstext -JM -R -Gblue -B -K -O >> ${map}
#����������
echo 103.993 27.553 | psxy -JM -R -Gred -Sa0.2i -B -K -O >> ${map}
#����ͨ������
# echo 103.717 27.338 | psxy -JM -R -Gblack -Ss0.15i -B -K -O >> ${map} 
# echo 104.048 27.625 | psxy -JM -R -Gblack -Ss0.15i -B -K -O >> ${map}
# pstext -JM -R -Gblack -B -K -O << ! >> ${map}
# 104.45 27.30 15 0 0 RT ZhaoTong
# 104.10 27.75 15 0 0 LT YiLiang
# !
# pstext -JM -R -Gblack -B -K -O << ! >> ${map}
# 102.25 29.45 15 0 0 LT XSHF
# 102.10 28.75 15 0 0 LT ANHF
# 102.65 27.35 15 0 0 LT ZMHF
# 103.05 26.05 15 0 0 LT XJF
# 102.45 26.05 15 0 0 LT PDHF 
# 103.15 27.75 15 0 0 LT LFF
# 103.25 27.35 15 0 0 LT ZTF
# 103.50 26.30 15 0 0 LT XWF
# !
#����ͼ��
pslegend -Dx3.3i/0i/2.2i/0.95i/BL -JM -R -Gwhite -F -K -O << ! >> ${map}
S 0.4i - 0.6i - 1.0p,red,- 0.8i Prov. boundary
S 0.4i - 0.6i - 1.0p,blue 0.8i fault
S 0.4i a 0.2i red red 0.8i earthquake
S 0.4i t 0.2i black black 0.8i station
!



#��������
#psxy river.txt -JM -R -W1.0p/blue  -B -O -K >> ${map}



