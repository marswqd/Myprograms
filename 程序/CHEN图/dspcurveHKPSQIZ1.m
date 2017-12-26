clear
load F:\地震数据\日本地震\处理结果\频散曲线\HKPS-QIZ\双台\earthquake1.25.txt
load F:\地震数据\日本地震\处理结果\频散曲线\HKPS-QIZ\双台\earthquake1.50.txt
load F:\地震数据\日本地震\处理结果\频散曲线\HKPS-QIZ\双台\earthquake2.25.txt
load F:\地震数据\日本地震\处理结果\频散曲线\HKPS-QIZ\双台\earthquake2.50.txt
load F:\地震数据\日本地震\处理结果\频散曲线\HKPS-QIZ\双台\earthquake3.25.txt
load F:\地震数据\日本地震\处理结果\频散曲线\HKPS-QIZ\双台\earthquake3.50.txt
semilogx(earthquake1_25(:,1),earthquake1_25(:,2),'k','LineWidth',1.5);
hold on
semilogx(earthquake2_25(:,1),earthquake2_25(:,2),'-.b','LineWidth',1.5);
semilogx(earthquake3_25(:,1),earthquake3_25(:,2),'r','LineWidth',1.5);
legend('earthquake1','earthquake2','earthquake3')
title('HKPS-QIZ(alpha=25)')

%semilogx(earthquake1_50(:,1),earthquake1_50(:,2),'k','LineWidth',1.5);
%hold on
%semilogx(earthquake2_50(:,1),earthquake2_50(:,2),'-.b','LineWidth',1.5);
%semilogx(earthquake3_50(:,1),earthquake3_50(:,2),'r','LineWidth',1.5);
%legend('earthquake1','earthquake2','earthquake3')
%title('HKPS-QIZ(alpha=50)')



%legend('A111A408','A001A209','A202L207','A704L228','K023L207','L209L228')

axis([4 50 2 5]);
xlabel('Period(s)')
ylabel('U(km/s)')