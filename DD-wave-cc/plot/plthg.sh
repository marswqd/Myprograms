#绘走时、震中距、方位角、震源深度统计图.
#!/bin/sh
tt=ttt-ylZJS.txt;d_tps=d_tylZJS.ps

distaz=staCT.txt
event=event.txt

#震中距-标定走时统计——走时曲线
psbasemap -JX6i/4i -R0/800/0/300 \
-B100:"DIST(km)":/50:"Travel Time(s)"::."Travel Time Curve":WSne -K > ${d_tps} 
awk '{if($6=="P") print $4,$2}' $tt | psxy -JX -R -Gblue -Sc0.05i -B -O -K >> ${d_tps}
awk '{if($6=="S") print $4,$2}' $tt | psxy -JX -R -Gblue -Sc0.05i -B -O -K >> ${d_tps}
#震中距-理论走时统计——走时曲线
awk '{if($6=="P") print $4,$3}' $tt| psxy -JX -R -W0.01i,red -B -O -K >> ${d_tps}
awk '{if($6=="S") print $4,$3}' $tt| psxy -JX -R -W0.01i,red -B -O -K >> ${d_tps}
#画标示
psxy -JX -R -Gblue -Sc0.05i -O -K << ! >> ${d_tps}
600 290
!
psxy -JX -R -Gred -Sc0.05i -O -K << ! >> ${d_tps}
600 270
!
pstext -JX -R -Gblue -O -K << ! >> ${d_tps}
700 290 15 0 0 CM real data
!
pstext -JX -R -Gred -O -K << ! >> ${d_tps}
700 270 15 0 0 CM theroy data
780 100  15 0 0 2 Pg
780 190  15 0 0 2 Sg
!

#台站方位角统计——相对于地震群质心
awk '{if($4<=500) print $5}' $distaz | pshistogram -G180/180/180 -L1.0 -W45 -R0/360/0/30 \
-B45:"AZ(o)":/5:"Number"::."Azimuth distribution":WSne -JX4i/4i -V -K > staz.ps
#震中距统计
awk '{if($4<=500) print $4}' $distaz | pshistogram -G180/180/180 -L1.0 -W100 -R0/800/0/20 \
-B100:"DIST(km)":/5:"Number"::."Epicentral distance distribution":WSne -JX6i/4i -V -K > dist.ps 
#震源深度统计
awk '{print $9}' $event | pshistogram -G180/180/180 -L1.0 -W5 -R0/20/0/800 \
-B5:"Depth(km)":/100:"Number"::."Depth distribution":WSne -JX4i/4i -V -K > evdp.ps

#震相震中距统计
awk '{if($6=="P") print $4}' $tt | pshistogram -G180/180/180 -Xa1.5i -L1.0 -W100 -R0/800/0/5500 \
-B100:"DIST(km)":/500:"Number":WSne -JX4i/4i -V -K > phase-dist.ps
awk '{if($6=="S") print $4}' $tt | pshistogram -G180/180/180 -Xa7i -L -W100 -R \
-B100:"DIST(km)":/500WSne -JX -V -K -O >> phase-dist.ps
pstext -JX11.7i/6i -R0/1/0/1 -Gblack -K -O << ! >> phase-dist.ps
0.5 1 35 0 1 CT Phase-distance distribution
0.45 0.8 20 0 1 CT P
0.92 0.8 20 0 1 CT S
!
#震相方位角统计
awk '{if($6=="P") print $5}' $tt | pshistogram -G180/180/180 -Xa1.5i -L1.0 -W45 -R0/360/0/2500 \
-B45:"AZ(o)":/500:"Number":WSne -JX4i/4i -V -K > phase-az.ps
awk '{if($6=="S") print $5}' $tt | pshistogram -G180/180/180 -Xa7i -L -W45 -R \
-B45:"AZ(o)":/500WSne -JX -V -K -O >> phase-az.ps
pstext -JX11.7i/6i -R0/1/0/1 -Gblack -K -O << ! >> phase-az.ps
0.5 1 35 0 1 CT Phase-azimuth distribution
0.45 0.8 20 0 1 CT P
0.92 0.8 20 0 1 CT S
!



