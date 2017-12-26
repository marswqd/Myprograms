clear
disp('START')
disp('     ')
%载入信号
sacname = 'A111.A602.2007.D.SAC.+';
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
zero = 0.1; perl = 4.0;  vell = 2.0; velh = 5.0; xlimit = [0 200];
dist = rhdr(51); perh = dist/10*1.5;
t1 = 0.9*perl; t2 = perl; t3 = perh; t4 = 1.1*perh;               %周期窗
f1 = 1/t4; f2 = 1/t3; f3 = 1/t2; f4 = 1/t1;                       %频率窗
tt2 = dist/velh; tt3 = dist/vell; tt1 = 0.9*tt2; tt4 = 1.1*tt3;   %走时窗
fprintf('Zero setting = %4.2f\n',zero)
fprintf('Velocity window = [ %6.4f  %6.4f ](km/s)\n',vell,velh)
fprintf('Travel Time window = [ %6.4f  %6.4f ](s)\n',tt2,tt3)
fprintf('Perior window = [ %6.4f  %6.4f ](s)\n',t2,t3)
fprintf('Freqence window = [ %6.4f  %6.4f ](Hz)\n',f2,f3)
disp('     ')
%画原始波形
NPTS = size(data,1);
t = (1:1:NPTS);
plot(t,data,'b');xlabel('走时');ylabel('振幅');title([sacname,'   DIST = ',num2str(dist),'km']);xlim(xlimit);
%信号S变换
disp('Begin S-Transform')
[st_data,st_times,st_freqs] = st(data);
contourf(st_times,st_freqs,abs(st_data));xlabel('走时');ylabel('频率');title('信号的S谱');axis([xlimit 0.0 0.5]);colorbar;
%时频滤波
disp('     ')
disp('Begin Time-frequency Filtering')
disp('     ')
[m,n] = size(st_data);
tf_data = st_data;
maxamp = 0.0;
for i = 1:m
    for j = 1:n
        if (st_freqs(i) >= f2) && (st_freqs(i) <= f3) && (st_times(j) >= tt2) && (st_times(j) <= tt3)
            if maxamp <= abs(tf_data(i,j)), maxamp = abs(tf_data(i,j)); end
        end
    end
end
zero = zero*maxamp;
for i = 1:m
    for j = 1:n
        if (st_freqs(i) >= f2) && (st_freqs(i) <= f3) && (st_times(j) >= tt2) && (st_times(j) <= tt3)
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
%波形比较
wname = ['ST.',sacname];
subplot(2,2,1);plot(t,data,'b');ylabel('振幅');title([sacname,'   DIST = ',num2str(dist),'km']);xlim(xlimit);
subplot(2,2,2);plot(t,ts_data,'r');title([wname,'   DIST = ',num2str(dist),'km']);xlim(xlimit);
subplot(2,2,3);plot(t,data,'b',t,ts_data,'r');xlabel('走时');ylabel('振幅');legend('原波形','滤波后波形');xlim(xlimit);
subplot(2,2,4);plot(t,data'-ts_data,'b');xlabel('走时');ylabel('振幅');legend('波形差');xlim(xlimit);
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