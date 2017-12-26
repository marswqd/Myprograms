#绘走时曲线.
#!/bin/sh

tt1=ttt-ylZJS.txt
tt2=ttt-yl.txt
d_tps=d_t12.ps

J=X6i/4i
R=0/500/0/200
#震中距-标定走时统计——走时曲线<=500km
psbasemap -J${J} -R${R} \
-B100:"DIST(km)":/50:"Travel Time(s)"::."Travel Time Curve":WSne -K > ${d_tps} 
awk '{if($6=="P" && $4<=500) print $4,$2}' $tt1 | psxy -J -R -Gblue -Sc0.05i -K -O >> ${d_tps}
awk '{if($6=="S" && $4<=500) print $4,$2}' $tt1 | psxy -J -R -Gblue -Sc0.05i -K -O >> ${d_tps}
#震中距-理论走时统计——走时曲线
awk '{if($6=="P" && $4<=500) print $4,$3}' $tt1 | psxy -J -R -W0.01i,green -K -O >> ${d_tps}
awk '{if($6=="S" && $4<=500) print $4,$3}' $tt1 | psxy -J -R -W0.01i,green -K -O >> ${d_tps}
awk '{if($6=="P" && $4<=500) print $4,$3}' $tt2 | psxy -J -R -W0.01i,red -K -O >> ${d_tps}
awk '{if($6=="S" && $4<=500) print $4,$3}' $tt2 | psxy -J -R -W0.01i,red -K -O >> ${d_tps}
#画标示
psxy -J -R -W0.01i,green -K -O << ! >> ${d_tps}
380 190
410 190
!
psxy -J -R -Gred -W0.01i,red -K -O << ! >> ${d_tps}
380 170
410 170
!
pstext -J -R -Ggreen -K -O << ! >> ${d_tps}
450 190 15 0 0 CM model 1
!
pstext -J -R -Gred -K -O << ! >> ${d_tps}
450 170  15 0 0 CM model 2
!
pstext -J -R -Gblack -K -O << ! >> ${d_tps}
480  60  15 0 0 2 Pg
480 114  15 0 0 2 Sg
!

awk 'ARGIND==1{a[$1 $6 $7]=$3} ARGIND==2{print $4,$2,$3,a[$1 $6 $7],$6}' $tt2 $tt1 > dt.tmp
awk '{if($5=="P" && $1<=400) { sum+=sqrt(($3-$2)*($3-$2))/$2;n+=1}}END{print "rmsP",sum/n}' dt.tmp
awk '{if($5=="P" && $1<=400) { sum+=sqrt(($4-$2)*($4-$2))/$2;n+=1}}END{print "rmsP",sum/n}' dt.tmp
awk '{if($5=="S" && $1<=400) { sum+=sqrt(($3-$2)*($3-$2))/$2;n+=1}}END{print "rmsS",sum/n}' dt.tmp
awk '{if($5=="S" && $1<=400) { sum+=sqrt(($4-$2)*($4-$2))/$2;n+=1}}END{print "rmsS",sum/n}' dt.tmp


