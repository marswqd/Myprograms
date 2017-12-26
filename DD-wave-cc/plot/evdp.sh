#绘走时、震中距、方位角、震源深度统计图.
#!/bin/sh

event=event.txt
evdp=evdp.ps

#震源深度统计
awk '{print $9}' $event | pshistogram -G180/180/180 -L1.0 -W5 -R0/20/0/800 \
-B5:"Depth(km)":/100:"Number"::."Depth distribution":WSne -JX4i/4i -V -K > $evdp




