#draw time-depth map
#!/bin/sh
psfile=T-D3.ps
ctloc=ct.reloc
ccloc=cc.reloc
ctccloc=ctcc.reloc

awk '{printf "%4d-%02d-%02dT%f:%f:%f %f %f %f \n",$11,$12,$13,$14,$15,$16,$4,$4,$17*0.065}' ${ctloc} > ctloc.tmp
awk '{printf "%4d-%02d-%02dT%f:%f:%f %f %f %f \n",$11,$12,$13,$14,$15,$16,$4,$4,$17*0.065}' ${ccloc} > ccloc.tmp
awk '{printf "%4d-%02d-%02dT%f:%f:%f %f %f %f \n",$11,$12,$13,$14,$15,$16,$4,$4,$17*0.065}' ${ctccloc} > ctccloc.tmp
#cat dep.new | awk '{print $2 }' | sort -nu > dep.tmp
makecpt -Cwysiwyg -T0/20/0.1 -I -Z > g.cpt
awk '{print $1,$2,$4}' ctloc.tmp | psxy -JX7i/-4i -R2012-09-07T11/2012-10-07T23/0/20 \
-Ba3d:"Time(day)":/5:"Depth(km)":WSen -Sc -Ggreen -K -P > ${psfile}
awk '{print $1,$2,$4}' ccloc.tmp | psxy -JX -R -B -Sc -Gblue -K -O -P >> ${psfile}
awk '{print $1,$2,$4}' ctccloc.tmp | psxy -JX -R -B -Sc -Gred -K -O -P >> ${psfile}
rm *.tmp