clear
disp('START')
disp('     ')
%载入信号
sacname = 'K030.K051.3-.SAC';
fid = fopen(sacname);
rhdr = fread(fid,70,'real*4');
ihdr = fread(fid,40,'integer*4');
chdr = fread(fid,192,'char');
data = fread(fid,'real*4');
fclose(fid);
fprintf('The input file is  %s\n',sacname)
fprintf('The DIST = %6.2fkm\n',rhdr(51))
fprintf('The maximum credible perior = %6.3fs\n',rhdr(51)/10)
disp('     ')
%参数设定
zero = 0.2; perl = 4.0;  vell = 2.0; velh = 5.0; xlimit = [0 200];
dist = rhdr(51); perh = dist/10*1.5;
t1 = perl; t2 = perh;               %周期窗
f1 = 1/t2; f2 = 1/t1;               %频率窗
tt1 = dist/velh; tt2 = dist/vell;   %走时窗
fprintf('Zero setting = %4.2f\n',zero)
fprintf('Velocity window = [ %6.4f  %6.4f ](km/s)\n',vell,velh)
fprintf('Travel Time window = [ %6.4f  %6.4f ](s)\n',tt1,tt2)
fprintf('Perior window = [ %6.4f  %6.4f ](s)\n',t1,t2)
fprintf('Freqence window = [ %6.4f  %6.4f ](Hz)\n',f1,f2)
disp('     ')
%画原始波形
NPTS = size(data,1);
t = (1:1:NPTS);
% plot(t,data,'b');xlabel('走时');ylabel('振幅');title([sacname,'   DIST = ',num2str(dist),'km']);xlim(xlimit);
%信号S变换
disp('Begin S-Transform')
[st_data,st_times,st_freqs] = st(data);
[m,n] = size(st_data);
% contourf(st_times,st_freqs,abs(st_data));xlabel('走时');ylabel('频率');title('信号的S谱');axis([xlimit 0.0 0.5]);colorbar;
% contourf(1./st_freqs(floor(f1/2*NPTS+1):ceil(f2*NPTS+1)),dist./st_times(floor(tt1/2):ceil(tt2*2)),abs(st_data(floor(f1/2*NPTS+1):ceil(f2*NPTS+1),floor(tt1/2):ceil(tt2*2)).'));
%时频滤波
disp('     ')
disp('Begin Time-frequency Filtering')
disp('     ')
tf_data = st_data;
maxamp = 0.0;
for i = 1:m
    for j = 1:n
        if (st_freqs(i) >= f1) && (st_freqs(i) <= f2) && (st_times(j) >= tt1) && (st_times(j) <= tt2)
            if maxamp <= abs(tf_data(i,j)), maxamp = abs(tf_data(i,j)); end
        end
    end
end
zero = zero*maxamp;
for i = 1:m
    for j = 1:n
        if (st_freqs(i) >= f1) && (st_freqs(i) <= f2) && (st_times(j) >= tt1) && (st_times(j) <= tt2)
            if abs(tf_data(i,j)) <= zero, tf_data(i,j) = 0.0+ 0.0*1i; end
        else
            tf_data(i,j) = 0.0+ 0.0*1i;
        end
    end
end
% contourf(st_times,st_freqs,abs(tf_data));xlabel('走时');ylabel('频率');title('滤波后S谱');axis([xlimit 0.0 0.5]);colorbar;            
%滤波信号S反变换
disp('Begin S-Inverse Transform')
disp('     ')
[ts_data,ts_all,fullstspe] = inverse_st(tf_data);
wname = ['ST.',sacname];
%% 波形比较
% subplot(2,2,1);plot(t,data,'b');ylabel('振幅');title([sacname,'   DIST = ',num2str(dist),'km']);xlim(xlimit);
% hold on;line([tt1,tt1],ylim,'Color','k','LineWidth',1.5);line([tt2,tt2],ylim,'Color','k','LineWidth',1.5);
% subplot(2,2,2);plot(t,ts_data,'r');title([wname,'   DIST = ',num2str(dist),'km']);xlim(xlimit);
% hold on;line([tt1,tt1],ylim,'Color','k','LineWidth',1.5);line([tt2,tt2],ylim,'Color','k','LineWidth',1.5);
% subplot(2,2,3);plot(t,data,'b',t,ts_data,'r');xlabel('走时');ylabel('振幅');legend('原波形','滤波后波形');xlim(xlimit);
% hold on;line([tt1,tt1],ylim,'Color','k','LineWidth',1.5);line([tt2,tt2],ylim,'Color','k','LineWidth',1.5);
% subplot(2,2,4);plot(t,data'-ts_data,'b');xlabel('走时');ylabel('振幅');legend('波形差');xlim(xlimit);
% hold on;line([tt1,tt1],ylim,'Color','k','LineWidth',1.5);line([tt2,tt2],ylim,'Color','k','LineWidth',1.5);
%生成滤波SAC文件
fid = fopen(wname, 'w');
fwrite(fid,rhdr,'real*4');
fwrite(fid,ihdr,'integer*4');
fwrite(fid,chdr,'char');
fwrite(fid,ts_data,'real*4');
fclose(fid);       
fprintf('The output file is  %s\n',wname)
disp('     ')
disp('END')
disp('     ')

% pause

%绘图
%% 信号S谱
contourf(st_times,st_freqs,abs(st_data));xlabel('走时');ylabel('频率');title('信号的S谱');axis([xlimit 0.0 0.5]);colorbar;
hold on;
line([tt1,tt1],[f1,f2],'Color','r','LineWidth',1.5);line([tt2,tt2],[f1,f2],'Color','r','LineWidth',1.5);
line([tt1,tt2],[f1,f1],'Color','r','LineWidth',1.5);line([tt1,tt2],[f2,f2],'Color','r','LineWidth',1.5);
%% 滤波后S谱
contourf(st_times,st_freqs,abs(tf_data));xlabel('走时');ylabel('频率');title('滤波后S谱');axis([xlimit 0.0 0.5]);colorbar; 
%% 波形比较
subplot(2,2,1);plot(t,data,'b');ylabel('振幅');title([sacname,'   DIST = ',num2str(dist),'km']);xlim(xlimit);
hold on;line([tt1,tt1],ylim,'Color','k','LineWidth',1.5);line([tt2,tt2],ylim,'Color','k','LineWidth',1.5);
subplot(2,2,2);plot(t,ts_data,'r');title([wname,'   DIST = ',num2str(dist),'km']);xlim(xlimit);
hold on;line([tt1,tt1],ylim,'Color','k','LineWidth',1.5);line([tt2,tt2],ylim,'Color','k','LineWidth',1.5);
subplot(2,2,3);plot(t,data,'b',t,ts_data,'r');xlabel('走时');ylabel('振幅');legend('原波形','滤波后波形');xlim(xlimit);
hold on;line([tt1,tt1],ylim,'Color','k','LineWidth',1.5);line([tt2,tt2],ylim,'Color','k','LineWidth',1.5);
subplot(2,2,4);plot(t,data'-ts_data,'b');xlabel('走时');ylabel('振幅');legend('波形差');xlim(xlimit);
hold on;line([tt1,tt1],ylim,'Color','k','LineWidth',1.5);line([tt2,tt2],ylim,'Color','k','LineWidth',1.5);
%% 周期-速度图
% contourf(1./st_freqs(floor(f1/2*NPTS+1):ceil(f2*NPTS+1)),dist./st_times(floor(tt1/2):ceil(tt2*2)),abs(st_data(floor(f1/2*NPTS+1):ceil(f2*NPTS+1),floor(tt1/2):ceil(tt2*2)).'));




