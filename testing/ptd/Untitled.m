cor = load('COR.TXT'); ab = load('DATA.TXT'); abns = load('DATANS.TXT');
tcor = load('TCOR.TXT');  line = load('line.TXT');
msc = load('FCORF.TXT');

plot(ab(:,1));hold on;plot(ab(:,2),'r');hold off;legend('data1','data2');%ԭʼ����
[Cxy,F] = mscohere(ab(:,1),ab(:,2),hanning(101),100,256,40);%matlab�����ƽ�����ֵ
mc = mscohere(ab(:,1),ab(:,2));
plot(msc(:,1),msc(:,4),'r',F,mc,'b');xlim([0,20]);legend('Mine','Matlab');title('ƽ�����ֵ');%ƽ�����ֵ

plot(abns(:,1));hold on;plot(abns(:,2),'r');hold off;legend('data1','data2');%ԭʼ����
[Cxy,F] = mscohere(abns(:,1),abns(:,2),hanning(101),100,256,40);%matlab�����ƽ�����ֵ
mc = mscohere(abns(:,1),abns(:,2));
plot(msc(:,1),msc(:,4),'r',F,mc,'b');xlim([0,20]);legend('Mine','Matlab');title('ƽ�����ֵ');%ƽ�����ֵ

plot(cor(:,1),cor(:,2)); %���ϵ��
plot(tcor(:,1),tcor(:,2)); %����غ���

plot(msc(:,1),msc(:,2));xlim([0,20]);title('��������'); %��������
plot(msc(:,1),msc(:,3),'r',line(:,1),line(:,3));xlim([0,20]);legend('�������λ','���ֱ��'); %�������λ


plot(line(:,1),line(:,2),'r',line(:,1),line(:,3));







