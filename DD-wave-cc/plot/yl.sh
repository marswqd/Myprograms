#!/bin/bash
#功能：使用GMT画彝良主震-台站分布图
#------------------------------------------------------
#从大网格数据中截取小网格
gmtset FRAME_WIDTH = 0.1c
gmtset TICK_LENGTH = 0.1c
gmtset PLOT_DEGREE_FORMAT = ddd:mmF
grd1=YNsrtm.grd
grd2=yl.grd
R=103.84/104.15/27.45/27.65
map=yl2.ps


grdcut ${grd1} -G${grd2} -R${R}
#创建调色板
grd2cpt ${grd2} -Crelief -S-1600/3200/10 -Z > yl.cpt
#令光从北方照射（方位角0度），增强立体感
grdgradient ${grd2} -Gyl.grad -A0 -Ne0.5 
#画出云南图
grdimage yl.grd -JM5.5i -R -B1/0.5:."Station Distribution":WSne \
-Cyl.cpt -Iyl.grad -K > ${map}
#画出色彩标尺
#psscale -D7i/3i/4i/0.3i -Cyl.cpt -I -E -B1000/:km: -K -O >> ${map}
#画出省界
psxy prov.txt -JM -R -W1.0p/red -B -K -O >> ${map}
#画主断层
# psxy ynmainfault.dat -JM -R -W1.0p/blue  -B -K -O >> yiliang.ps
# #画出主断层的标示号
# pstext -JM -R -Gblack -B -K -O -P << ! >> yiliang.ps
# 98.685   26.657  17 0 1 2 A
# 99.933   24.896  17 0 1 2 B
# 100.727  27.069  17 0 1 2 C
# 101.602  24.221  17 0 1 2 D
# 103.224  26.501  17 0 1 2 E
# !
#画出断层（全国断层，但是由于范围问题只能显示97/107/21/30内的断层）
#psxy ynfault.txt -JM -R -W1.0p/blue -B -K -O >> ${map}
#彝良附近断层：昭通—鲁甸断裂
psxy -JM -R -W1.8p/black -B -K -O << ! >> ${map}
104.0999 27.78207
104.0708 27.74029
104.0195 27.69116
103.9678 27.6348
103.9118 27.58001
103.8603 27.53618
103.8008 27.47577
103.7505 27.43017
103.7127 27.39774
103.6732 27.36072
103.6341 27.32013
103.595 27.27862
103.5648 27.25641
103.5406 27.23541
103.534 27.22878
103.534 27.22878
!
#画事件
makecpt -Crainbow -T0/16/2 -I > g.cpt
awk '{print $8,$7,$9,$10*0.08}' event.txt | psxy -JM -Cg.cpt -R -Sc -O -K -N >> ${map}
awk '{print $8,$7,$10*0.08}' eventbefor.txt | psxy -JM -R -Gblack -Sc -O -K -N >> ${map}
#画台站
#区域台
awk '{print $3,$2}' staqy.txt | psxy -JM -R -Gblack -St0.15i -B -K -O >> ${map}
awk '{print $3,$2+0.1,8,0,1,"CB",$1}' staqy.txt | pstext -JM -R -Gblack -B -K -O >> ${map}
#流动台
awk '{print $3,$2}' stals.txt | psxy -JM -R -Gblack -Sd0.15i -B -K -O >> ${map}
awk '{print $3,$2+0.06,8,0,1,"CB",$1}' stals.txt | pstext -JM -R -Gblack -B -K -O >> ${map}
#画彝良主震
echo 103.993 27.553 | psxy -JM -R -Gred -Sc0.15i -B -K -O >> ${map}
#画昭通、彝良
echo 103.717 27.338 | psxy -JM -R -Gred -Sa0.15i -B -K -O >> ${map} 
echo 104.048 27.625 | psxy -JM -R -Gred -Sa0.15i -B -K -O >> ${map}
pstext -JM -R -Gblack -B -K -O << ! >> ${map}
103.717 27.3 8 0 1 RT ZhaoTong
104.048 27.6 8 0 1 LT YiLiang
!
#画出河流
#psxy river.txt -JM -R -W1.0p/blue  -B -O -K >> ${map}



