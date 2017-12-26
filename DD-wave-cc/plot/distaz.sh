#绘台站-震中距+方位角统计图——相对于地震群质心.
#!/bin/sh

distaz=staCT.txt
st_dist=st_dist.ps
st_az=st_az.ps

#震中距统计
awk '{if($4<=500) print $4}' $distaz | pshistogram -G180/180/180 -L1.0 -W100 -R0/500/0/20 \
-B100:"DIST(km)":/5:"Number"::."Epicentral distance distribution":WSne -JX6i/4i -V -K > $st_dist 
#方位角统计
awk '{if($4<=500) print $5}' $distaz | pshistogram -G180/180/180 -L1.0 -W45 -R0/360/0/25 \
-B45:"AZ(o)":/5:"Number"::."Azimuth distribution":WSne -JX4i/4i -V -K > $st_az







