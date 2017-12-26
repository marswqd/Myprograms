#绘走时曲线.
#!/bin/sh

tt=ttt-yl.txt
d_tps=d_tyl.ps

#震中距-标定走时统计——走时曲线<=500km
psbasemap -JX6i/4i -R0/500/0/200 \
-B100:"DIST(km)":/50:"Travel Time(s)"::."Travel Time Curve":WSne -K > ${d_tps} 
awk '{if($6=="P" && $4<=500) print $4,$2}' $tt | psxy -JX -R -Gblue -Sc0.05i -B -O -K >> ${d_tps}
awk '{if($6=="S" && $4<=500) print $4,$2}' $tt | psxy -JX -R -Gblue -Sc0.05i -B -O -K >> ${d_tps}
#震中距-理论走时统计——走时曲线
awk '{if($6=="P" && $4<=500) print $4,$3}' $tt| psxy -JX -R -W0.01i,red -B -O -K >> ${d_tps}
awk '{if($6=="S" && $4<=500) print $4,$3}' $tt| psxy -JX -R -W0.01i,red -B -O -K >> ${d_tps}
#画标示
psxy -JX -R -Gblue -Sc0.05i -O -K << ! >> ${d_tps}
400 190
!
psxy -JX -R -Gred -Sc0.05i -O -K << ! >> ${d_tps}
400 170
!
pstext -JX -R -Gblue -O -K << ! >> ${d_tps}
450 190 15 0 0 CM real data
!
pstext -JX -R -Gred -O -K << ! >> ${d_tps}
450 170  15 0 0 CM theroy data
480  60  15 0 0 2 Pg
480 114  15 0 0 2 Sg
!




