clear
%本程序是画日本地震HKPS-QIZ台单台频散曲线的图。
load F:\地震数据\日本地震\处理结果\频散曲线\HKPS-QIZ\单台\HKPSLHZearthquake1alhpa25.dsp.txt
load F:\地震数据\日本地震\处理结果\频散曲线\HKPS-QIZ\单台\HKPSLHZearthquake2alhpa25.dsp.txt
load F:\地震数据\日本地震\处理结果\频散曲线\HKPS-QIZ\单台\HKPSLHZearthquake3alpha25.dsp.txt
load F:\地震数据\日本地震\处理结果\频散曲线\HKPS-QIZ\单台\HKPSLHZearthquake1alhpa50.dsp.txt
load F:\地震数据\日本地震\处理结果\频散曲线\HKPS-QIZ\单台\HKPSLHZearthquake2alpha50.dsp.txt
load F:\地震数据\日本地震\处理结果\频散曲线\HKPS-QIZ\单台\HKPSLHZearthquake3alpha50.dsp.txt

load F:\地震数据\日本地震\处理结果\频散曲线\HKPS-QIZ\单台\QIZLHZearthquake1alhpa25.dsp.txt
load F:\地震数据\日本地震\处理结果\频散曲线\HKPS-QIZ\单台\QIZLHZearthquake2alpha25.dsp.txt
load F:\地震数据\日本地震\处理结果\频散曲线\HKPS-QIZ\单台\QIZLHZearthquake3alpha25.dsp.txt
load F:\地震数据\日本地震\处理结果\频散曲线\HKPS-QIZ\单台\QIZLHZearthquake1alhpa50.dsp.txt
load F:\地震数据\日本地震\处理结果\频散曲线\HKPS-QIZ\单台\QIZLHZearthquake2alpha50.dsp.txt
load F:\地震数据\日本地震\处理结果\频散曲线\HKPS-QIZ\单台\QIZLHZearthquake3alpha50.dsp.txt

load F:\地震数据\日本地震\处理结果\频散曲线\HKPS-QIZ\双台\earthquake1.25.txt
load F:\地震数据\日本地震\处理结果\频散曲线\HKPS-QIZ\双台\earthquake1.50.txt
load F:\地震数据\日本地震\处理结果\频散曲线\HKPS-QIZ\双台\earthquake2.25.txt
load F:\地震数据\日本地震\处理结果\频散曲线\HKPS-QIZ\双台\earthquake2.50.txt
load F:\地震数据\日本地震\处理结果\频散曲线\HKPS-QIZ\双台\earthquake3.25.txt
load F:\地震数据\日本地震\处理结果\频散曲线\HKPS-QIZ\双台\earthquake3.50.txt

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%HKPS单台频散，alpha=25
%semilogx(HKPSLHZearthquake1alhpa25_dsp(:,1),HKPSLHZearthquake1alhpa25_dsp(:,2),'k','LineWidth',1.5);
%hold on
%semilogx(HKPSLHZearthquake2alhpa25_dsp(:,1),HKPSLHZearthquake2alhpa25_dsp(:,2),'-.b','LineWidth',1.5);
%semilogx(HKPSLHZearthquake3alpha25_dsp(:,1),HKPSLHZearthquake3alpha25_dsp(:,2),'r','LineWidth',1.5);
%legend('earthquake1','earthquake2','earthquake3')
%title('HKPS(alpha=25)')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%HKPS单台频散，alpha=50
%semilogx(HKPSLHZearthquake1alhpa50_dsp(:,1),HKPSLHZearthquake1alhpa50_dsp(:,2),'k','LineWidth',1.5);
%hold on
%semilogx(HKPSLHZearthquake2alpha50_dsp(:,1),HKPSLHZearthquake2alpha50_dsp(:,2),'-.b','LineWidth',1.5);
%semilogx(HKPSLHZearthquake3alpha50_dsp(:,1),HKPSLHZearthquake3alpha50_dsp(:,2),'r','LineWidth',1.5);
%legend('earthquake1','earthquake2','earthquake3')
%title('HKPS(alpha=50)')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%QIZ单台频散，alpha=25
% semilogx(QIZLHZearthquake1alhpa25_dsp(:,1),QIZLHZearthquake1alhpa25_dsp(:,2),'k','LineWidth',1.5);
% hold on
% semilogx(QIZLHZearthquake2alpha25_dsp(:,1),QIZLHZearthquake2alpha25_dsp(:,2),'-.b','LineWidth',1.5);
% semilogx(QIZLHZearthquake3alpha25_dsp(:,1),QIZLHZearthquake3alpha25_dsp(:,2),'r','LineWidth',1.5);
% legend('earthquake1','earthquake2','earthquake3')
% title('QIZ(alpha=25)')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%QIZ单台频散，alpha=50
% semilogx(QIZLHZearthquake1alhpa50_dsp(:,1),QIZLHZearthquake1alhpa50_dsp(:,2),'k','LineWidth',1.5);
% hold on
% semilogx(QIZLHZearthquake2alpha50_dsp(:,1),QIZLHZearthquake2alpha50_dsp(:,2),'-.b','LineWidth',1.5);
% semilogx(QIZLHZearthquake3alpha50_dsp(:,1),QIZLHZearthquake3alpha50_dsp(:,2),'r','LineWidth',1.5);
% legend('earthquake1','earthquake2','earthquake3')
% title('QIZ(alpha=50)')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%earthquake1，alpha=25
% semilogx(HKPSLHZearthquake1alhpa25_dsp(:,1),HKPSLHZearthquake1alhpa25_dsp(:,2),'-.b','LineWidth',1.5);
% hold on
% semilogx(QIZLHZearthquake1alhpa25_dsp(:,1),QIZLHZearthquake1alhpa25_dsp(:,2),'k','LineWidth',1.5);
% semilogx(earthquake1_25(:,1),earthquake1_25(:,2),'r','LineWidth',1.5);
% legend('HKPS','QIZ','HKPS-QIZ')
% title('earthquake1(alpha=25)')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%earthquake1，alpha=50
% semilogx(HKPSLHZearthquake1alhpa50_dsp(:,1),HKPSLHZearthquake1alhpa50_dsp(:,2),'-.b','LineWidth',1.5);
% hold on
% semilogx(QIZLHZearthquake1alhpa50_dsp(:,1),QIZLHZearthquake1alhpa50_dsp(:,2),'k','LineWidth',1.5);
% semilogx(earthquake1_50(:,1),earthquake1_50(:,2),'r','LineWidth',1.5);
% legend('HKPS','QIZ','HKPS-QIZ')
% title('earthquake1(alpha=50)')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%earthquake2，alpha=25
% semilogx(HKPSLHZearthquake2alhpa25_dsp(:,1),HKPSLHZearthquake2alhpa25_dsp(:,2),'-.b','LineWidth',1.5);
% hold on
% semilogx(QIZLHZearthquake2alpha25_dsp(:,1),QIZLHZearthquake2alpha25_dsp(:,2),'k','LineWidth',1.5);
% semilogx(earthquake2_25(:,1),earthquake2_25(:,2),'r','LineWidth',1.5);
% legend('HKPS','QIZ','HKPS-QIZ')
% title('earthquake2(alpha=25)')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%earthquake2，alpha=50
% semilogx(HKPSLHZearthquake2alpha50_dsp(:,1),HKPSLHZearthquake2alpha50_dsp(:,2),'-.b','LineWidth',1.5);
% hold on
% semilogx(QIZLHZearthquake1alhpa50_dsp(:,1),QIZLHZearthquake1alhpa50_dsp(:,2),'k','LineWidth',1.5);
% semilogx(earthquake2_50(:,1),earthquake2_50(:,2),'r','LineWidth',1.5);
% legend('HKPS','QIZ','HKPS-QIZ')
% title('earthquake2(alpha=50)')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%earthquake3，alpha=25
% semilogx(HKPSLHZearthquake3alpha25_dsp(:,1),HKPSLHZearthquake3alpha25_dsp(:,2),'-.b','LineWidth',1.5);
% hold on
% semilogx(QIZLHZearthquake3alpha25_dsp(:,1),QIZLHZearthquake3alpha25_dsp(:,2),'k','LineWidth',1.5);
% semilogx(earthquake3_25(:,1),earthquake3_25(:,2),'r','LineWidth',1.5);
% legend('HKPS','QIZ','HKPS-QIZ')
% title('earthquake3(alpha=25)')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%earthquake3，alpha=50
semilogx(HKPSLHZearthquake3alpha50_dsp(:,1),HKPSLHZearthquake3alpha50_dsp(:,2),'-.b','LineWidth',1.5);
hold on
semilogx(QIZLHZearthquake3alpha50_dsp(:,1),QIZLHZearthquake3alpha50_dsp(:,2),'k','LineWidth',1.5);
semilogx(earthquake1_50(:,1),earthquake1_50(:,2),'r','LineWidth',1.5);
legend('HKPS','QIZ','HKPS-QIZ')
title('earthquake3(alpha=50)')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



axis([4 100 2 5]);
xlabel('Period(s)')
ylabel('U(km/s)')