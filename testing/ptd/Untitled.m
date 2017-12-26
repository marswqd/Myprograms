cor = load('COR.TXT'); ab = load('DATA.TXT'); abns = load('DATANS.TXT');
tcor = load('TCOR.TXT');  line = load('line.TXT');
msc = load('FCORF.TXT');

plot(ab(:,1));hold on;plot(ab(:,2),'r');hold off;legend('data1','data2');%原始数据
[Cxy,F] = mscohere(ab(:,1),ab(:,2),hanning(101),100,256,40);%matlab计算的平方相干值
mc = mscohere(ab(:,1),ab(:,2));
plot(msc(:,1),msc(:,4),'r',F,mc,'b');xlim([0,20]);legend('Mine','Matlab');title('平方相干值');%平方相干值

plot(abns(:,1));hold on;plot(abns(:,2),'r');hold off;legend('data1','data2');%原始数据
[Cxy,F] = mscohere(abns(:,1),abns(:,2),hanning(101),100,256,40);%matlab计算的平方相干值
mc = mscohere(abns(:,1),abns(:,2));
plot(msc(:,1),msc(:,4),'r',F,mc,'b');xlim([0,20]);legend('Mine','Matlab');title('平方相干值');%平方相干值

plot(cor(:,1),cor(:,2)); %相关系数
plot(tcor(:,1),tcor(:,2)); %互相关函数

plot(msc(:,1),msc(:,2));xlim([0,20]);title('互相关振幅'); %互相关振幅
plot(msc(:,1),msc(:,3),'r',line(:,1),line(:,3));xlim([0,20]);legend('互相关相位','拟合直线'); %互相关相位


plot(line(:,1),line(:,2),'r',line(:,1),line(:,3));







