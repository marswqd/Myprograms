#!/bin/sh
awk '{printf "%4d-%02d %f %f %f \n",$11,$12,$4,$4,$17*0.06}' hypoDD.loc > dep.old
awk '{printf "%4d-%02d %f %f %f \n",$11,$12,$4,$4,$17*0.06}' hypoDD.reloc > dep.new
cat dep.new | awk '{print $2 }' | sort -nu > dep.tmp
makecpt -Crainbow -Tdep.tmp -Z > g.cpt
#画原始地震时间-深度图
psbasemap -R2008-12T/2012-05T/0/20 -Ba6Of31/a5:,"-km":WSen -JX7i/-4i -K -P -V > dep.wqd.ps
psxy dep.old -R -JX -B -Sci -Cdepth1.cpt -G255/0/0 -K -O -P -V >> dep.old.ps
#画定位之后时间-深度图
psbasemap -R2008-12T/2012-05T/0/20 -Ba12Of31/a5:,"-km":WSen -JX7i/-4i -K -P -V > dep.wqd2.ps
psxy dep.new -R -JX -B -Sci -Cdepth1.cpt -G255/0/0 -K -O -P -V  >> dep.new.ps
rm dep.tmp