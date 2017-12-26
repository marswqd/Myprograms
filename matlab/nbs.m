clear
%% 1-2 07s
clear
sacname1 = 'A101.A608.-.SAC.07.S';
sacname2 = 'A101.A608.+.SAC.07.S';
% load A101.A608.-.SAC.07.S.txt;
% sdata1 = A101_A608___SAC_07_S;
% load A101.A608.+.SAC.07.S.txt;
% sdata2 = A101_A608___SAC_07_S;
fid = fopen(sacname1);
rhdr = fread(fid,70,'real*4');
ihdr = fread(fid,40,'integer*4');
chdr = fread(fid,192,'char');
data1 = fread(fid,'real*4');
fclose(fid);
fid = fopen(sacname2);
rhdr = fread(fid,70,'real*4');
ihdr = fread(fid,40,'integer*4');
chdr = fread(fid,192,'char');
data2 = fread(fid,'real*4');
fclose(fid);
dist = rhdr(51);
NPTS = ihdr(10);
t=0:1:NPTS-1;
plot(t,data1,'r',t,data2,'b','LineWidth',2.0);
axis([dist/5 dist/2 -1 1]);legend('非因果信号','因果信号','fontsize',16);
title({['A101.A608.SAC  DIST = ',num2str(dist),'km'];['7s周期的窄带滤波比较']},'fontsize',16);
xlabel('走时（信号窗）','fontsize',16);ylabel('归一化振幅','fontsize',16);
%% 1-2 10s
clear
sacname1 = 'A101.A608.-.SAC.10.S';
sacname2 = 'A101.A608.+.SAC.10.S';
% load A101.A608.-.SAC.10.S.txt;
% sdata1 = A101_A608___SAC_10_S;
% load A101.A608.+.SAC.07.S.txt;
% sdata2 = A101_A608___SAC_10_S;
fid = fopen(sacname1);
rhdr = fread(fid,70,'real*4');
ihdr = fread(fid,40,'integer*4');
chdr = fread(fid,192,'char');
data1 = fread(fid,'real*4');
fclose(fid);
fid = fopen(sacname2);
rhdr = fread(fid,70,'real*4');
ihdr = fread(fid,40,'integer*4');
chdr = fread(fid,192,'char');
data2 = fread(fid,'real*4');
fclose(fid);
dist = rhdr(51);
NPTS = ihdr(10);
t=0:1:NPTS-1;
plot(t,data1,'r',t,data2,'b','LineWidth',2.0);
axis([dist/5 dist/2 -1 1]);legend('非因果信号','因果信号','fontsize',16);
title({['A101.A608.SAC  DIST = ',num2str(dist),'km'];['10s周期的窄带滤波比较']},'fontsize',16);
xlabel('走时（信号窗）','fontsize',16);ylabel('归一化振幅','fontsize',16);
%% 1-2 20s
clear
sacname1 = 'A101.A608.-.SAC.20.S';
sacname2 = 'A101.A608.+.SAC.20.S';
% load A101.A608.-.SAC.07.S.txt;
% sdata1 = A101_A608___SAC_07_S;
% load A101.A608.+.SAC.07.S.txt;
% sdata2 = A101_A608___SAC_07_S;
fid = fopen(sacname1);
rhdr = fread(fid,70,'real*4');
ihdr = fread(fid,40,'integer*4');
chdr = fread(fid,192,'char');
data1 = fread(fid,'real*4');
fclose(fid);
fid = fopen(sacname2);
rhdr = fread(fid,70,'real*4');
ihdr = fread(fid,40,'integer*4');
chdr = fread(fid,192,'char');
data2 = fread(fid,'real*4');
fclose(fid);
dist = rhdr(51);
NPTS = ihdr(10);
t=0:1:NPTS-1;
plot(t,data1,'r',t,data2,'b','LineWidth',2.0);
axis([dist/5 dist/2 -1 1]);legend('非因果信号','因果信号','fontsize',16);
title({['A101.A608.SAC  DIST = ',num2str(dist),'km'];['20s周期的窄带滤波比较']},'fontsize',16);
xlabel('走时（信号窗）','fontsize',16);ylabel('归一化振幅','fontsize',16);


%% 2-3 07s
clear
sacname1 = 'A608.L236.-.SAC.07.S';
sacname2 = 'A608.L236.+.SAC.07.S';
% load A101.A608.-.SAC.07.S.txt;
% sdata1 = A101_A608___SAC_07_S;
% load A101.A608.+.SAC.07.S.txt;
% sdata2 = A101_A608___SAC_07_S;
fid = fopen(sacname1);
rhdr = fread(fid,70,'real*4');
ihdr = fread(fid,40,'integer*4');
chdr = fread(fid,192,'char');
data1 = fread(fid,'real*4');
fclose(fid);
fid = fopen(sacname2);
rhdr = fread(fid,70,'real*4');
ihdr = fread(fid,40,'integer*4');
chdr = fread(fid,192,'char');
data2 = fread(fid,'real*4');
fclose(fid);
dist = rhdr(51);
NPTS = ihdr(10);
t=0:1:NPTS-1;
plot(t,data1,'r',t,data2,'b','LineWidth',2.0);
axis([dist/5 dist/2 -1 1]);legend('非因果信号','因果信号','fontsize',16);
title({['A608.L236.SAC  DIST = ',num2str(dist),'km'];['7s周期的窄带滤波比较']},'fontsize',16);
xlabel('走时（信号窗）','fontsize',16);ylabel('归一化振幅','fontsize',16);
%% 2-3 10s
clear
sacname1 = 'A608.L236.-.SAC.10.S';
sacname2 = 'A608.L236.+.SAC.10.S';
% load A101.A608.-.SAC.10.S.txt;
% sdata1 = A101_A608___SAC_10_S;
% load A101.A608.+.SAC.07.S.txt;
% sdata2 = A101_A608___SAC_10_S;
fid = fopen(sacname1);
rhdr = fread(fid,70,'real*4');
ihdr = fread(fid,40,'integer*4');
chdr = fread(fid,192,'char');
data1 = fread(fid,'real*4');
fclose(fid);
fid = fopen(sacname2);
rhdr = fread(fid,70,'real*4');
ihdr = fread(fid,40,'integer*4');
chdr = fread(fid,192,'char');
data2 = fread(fid,'real*4');
fclose(fid);
dist = rhdr(51);
NPTS = ihdr(10);
t=0:1:NPTS-1;
plot(t,data1,'r',t,data2,'b','LineWidth',2.0);
axis([dist/5 dist/2 -1 1]);legend('非因果信号','因果信号','fontsize',16);
title({['A608.L236.SAC  DIST = ',num2str(dist),'km'];['10s周期的窄带滤波比较']},'fontsize',16);
xlabel('走时（信号窗）','fontsize',16);ylabel('归一化振幅','fontsize',16);
%% 2-3 20s
clear
sacname1 = 'A608.L236.-.SAC.20.S';
sacname2 = 'A608.L236.+.SAC.20.S';
% load A101.A608.-.SAC.07.S.txt;
% sdata1 = A101_A608___SAC_07_S;
% load A101.A608.+.SAC.07.S.txt;
% sdata2 = A101_A608___SAC_07_S;
fid = fopen(sacname1);
rhdr = fread(fid,70,'real*4');
ihdr = fread(fid,40,'integer*4');
chdr = fread(fid,192,'char');
data1 = fread(fid,'real*4');
fclose(fid);
fid = fopen(sacname2);
rhdr = fread(fid,70,'real*4');
ihdr = fread(fid,40,'integer*4');
chdr = fread(fid,192,'char');
data2 = fread(fid,'real*4');
fclose(fid);
dist = rhdr(51);
NPTS = ihdr(10);
t=0:1:NPTS-1;
plot(t,data1,'r',t,data2,'b','LineWidth',2.0);
axis([dist/5 dist/2 -1 1]);legend('非因果信号','因果信号','fontsize',16);
title({['A608.L236.SAC  DIST = ',num2str(dist),'km'];['20s周期的窄带滤波比较']},'fontsize',16);
xlabel('走时（信号窗）','fontsize',16);ylabel('归一化振幅','fontsize',16);

%% 1-3 07s
clear
sacname1 = 'A101.L236.-.SAC.07.S';
sacname2 = 'A101.L236.+.SAC.07.S';
% load A101.A608.-.SAC.07.S.txt;
% sdata1 = A101_A608___SAC_07_S;
% load A101.A608.+.SAC.07.S.txt;
% sdata2 = A101_A608___SAC_07_S;
fid = fopen(sacname1);
rhdr = fread(fid,70,'real*4');
ihdr = fread(fid,40,'integer*4');
chdr = fread(fid,192,'char');
data1 = fread(fid,'real*4');
fclose(fid);
fid = fopen(sacname2);
rhdr = fread(fid,70,'real*4');
ihdr = fread(fid,40,'integer*4');
chdr = fread(fid,192,'char');
data2 = fread(fid,'real*4');
fclose(fid);
dist = rhdr(51);
NPTS = ihdr(10);
t=0:1:NPTS-1;
plot(t,data1,'r',t,data2,'b','LineWidth',2.0);
axis([dist/5 dist/2 -1 1]);legend('非因果信号','因果信号','fontsize',16);
title({['A101.L236.SAC  DIST = ',num2str(dist),'km'];['7s周期的窄带滤波比较']},'fontsize',16);
xlabel('走时（信号窗）','fontsize',16);ylabel('归一化振幅','fontsize',16);
%% 1-3 10s
clear
sacname1 = 'A101.L236.-.SAC.10.S';
sacname2 = 'A101.L236.+.SAC.10.S';
% load A101.A608.-.SAC.10.S.txt;
% sdata1 = A101_A608___SAC_10_S;
% load A101.A608.+.SAC.07.S.txt;
% sdata2 = A101_A608___SAC_10_S;
fid = fopen(sacname1);
rhdr = fread(fid,70,'real*4');
ihdr = fread(fid,40,'integer*4');
chdr = fread(fid,192,'char');
data1 = fread(fid,'real*4');
fclose(fid);
fid = fopen(sacname2);
rhdr = fread(fid,70,'real*4');
ihdr = fread(fid,40,'integer*4');
chdr = fread(fid,192,'char');
data2 = fread(fid,'real*4');
fclose(fid);
dist = rhdr(51);
NPTS = ihdr(10);
t=0:1:NPTS-1;
plot(t,data1,'r',t,data2,'b','LineWidth',2.0);
axis([dist/5 dist/2 -1 1]);legend('非因果信号','因果信号','fontsize',16);
title({['A101.L236.SAC  DIST = ',num2str(dist),'km'];['10s周期的窄带滤波比较']},'fontsize',16);
xlabel('走时（信号窗）','fontsize',16);ylabel('归一化振幅','fontsize',16);
%% 1-3 20s
clear
sacname1 = 'A101.L236.-.SAC.20.S';
sacname2 = 'A101.L236.+.SAC.20.S';
% load A101.A608.-.SAC.07.S.txt;
% sdata1 = A101_A608___SAC_07_S;
% load A101.A608.+.SAC.07.S.txt;
% sdata2 = A101_A608___SAC_07_S;
fid = fopen(sacname1);
rhdr = fread(fid,70,'real*4');
ihdr = fread(fid,40,'integer*4');
chdr = fread(fid,192,'char');
data1 = fread(fid,'real*4');
fclose(fid);
fid = fopen(sacname2);
rhdr = fread(fid,70,'real*4');
ihdr = fread(fid,40,'integer*4');
chdr = fread(fid,192,'char');
data2 = fread(fid,'real*4');
fclose(fid);
dist = rhdr(51);
NPTS = ihdr(10);
t=0:1:NPTS-1;
plot(t,data1,'r',t,data2,'b','LineWidth',2.0);
axis([dist/5 dist/2 -1 1]);legend('非因果信号','因果信号','fontsize',16);
title({['A101.L236.SAC  DIST = ',num2str(dist),'km'];['20s周期的窄带滤波比较']},'fontsize',16);
xlabel('走时（信号窗）','fontsize',16);ylabel('归一化振幅','fontsize',16);






