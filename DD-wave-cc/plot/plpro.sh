#!/bin/sh

#gmtset ANOT_FONT_SIZE 12 LABEL_FONT_SIZE 12 ANOT_FONT 1 LABEL_FONT 1
#设置网格线的属性，为红色虚线
#gmtset GRID_PEN_PRIMARY= 0.35p,255/0/0,2_1_0.25_1:0

#地震目录事件
psbasemap -JX6i/4i -R-10/20/-30/0 \
-B2:"Distant(km)":/5:"Depth(km)"::."Profile":WSne -K > proA.ps 
#画尺寸标尺
psxy -JX -R -Sc -N -Gblue -O -K << ! >> proA.ps
12 -26 0.05
13 -26 0.1
14 -26 0.15
15 -26 0.20
16 -26 0.25
17 -26 0.30
18 -26 0.35
19 -26 0.40
!
pstext -JX -R -Gblue -O -K << ! >> proA.ps
11 -28 7 0 1 2 M
12 -28 7 0 1 2 1
13 -28 7 0 1 2 2
14 -28 7 0 1 2 3
15 -28 7 0 1 2 4 
16 -28 7 0 1 2 5 
17 -28 7 0 1 2 6
18 -28 7 0 1 2 7
19 -28 7 0 1 2 8
!
cp proA.ps proAloc.ps
#地震目录事件
awk '{print $1,$3*-1,$4*0.05}' proe.txt | psxy -JX -R -Gblue -Sc -B -O -K -N >> proA.ps
#重定位事件与地震目录事件的比较
awk '{print $1,$3*-1,$4*0.05}' proloco.txt | psxy -JX -R -Gblue -Sc -K -O >> proAloc.ps
awk '{print $1,$3*-1,$4*0.05}' proloc.txt | psxy -JX -R -Gred -Sc -K -O >> proAloc.ps
psxy -JX -R -Sc -N -Gblue -O -K << ! >> proAloc.ps
19 -22 0.3
!
psxy -JX -R -Sc -N -Gred -O -K << ! >> proAloc.ps
19 -24 0.3
!
pstext -JX -R -N -Gblue -O -K << ! >> proAloc.ps
15 -22.3 10 0 1 2 Original Event
!
pstext -JX -R -N -Gred -O -K << ! >> proAloc.ps
15 -24.3 10 0 1 2 Relocation Event
!






