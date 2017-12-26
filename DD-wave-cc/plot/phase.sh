#绘震相-震中距+方位角统计图.
#!/bin/sh

tt=ttt-ylZJS.txt
phase_dist=phase_dist.ps
phase_az=phase_az.ps

#震相震中距统计
awk '{if($6=="P" && $4<=500) print $4}' $tt | pshistogram -G180/180/180 -Xa1.5i -L1.0 -W100 -R0/500/0/5500 \
-B100:"DIST(km)":/500:"Number":WSne -JX4i/4i -V -K > $phase_dist
awk '{if($6=="S" && $4<=500) print $4}' $tt | pshistogram -G180/180/180 -Xa7i -L -W100 -R \
-B100:"DIST(km)":/500WSne -JX -V -K -O >> $phase_dist
pstext -JX11.7i/6i -R0/1/0/1 -Gblack -K -O << ! >> $phase_dist
0.5 1 35 0 1 CT Phase-distance distribution
0.45 0.8 20 0 1 CT P
0.92 0.8 20 0 1 CT S
!
#震相方位角统计
awk '{if($6=="P" && $4<=500) print $5}' $tt | pshistogram -G180/180/180 -Xa1.5i -L1.0 -W45 -R0/360/0/2000 \
-B45:"AZ(o)":/500:"Number":WSne -JX4i/4i -V -K > $phase_az
awk '{if($6=="S" && $4<=500) print $5}' $tt | pshistogram -G180/180/180 -Xa7i -L -W45 -R \
-B45:"AZ(o)":/500WSne -JX -V -K -O >> $phase_az
pstext -JX11.7i/6i -R0/1/0/1 -Gblack -K -O << ! >> $phase_az
0.5 1 35 0 1 CT Phase-azimuth distribution
0.45 0.8 20 0 1 CT P
0.92 0.8 20 0 1 CT S
!



