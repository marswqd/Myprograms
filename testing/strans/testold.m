%载入信号,设定参数
%load('A111.A602.2007.D.SAC.-.ASC');
%signal=A111A602(1:3001,1);
load('A111.A602.2007.D.SAC.+.ASC');
nl = 1;nr = 40;len = 1500; 
%sl = 41;sr = ; 
%data(len) = 0;
data = A111_A602_2007_D_SAC__(:,1);
len = size(data,1);
t = (1:1:len);
plot(t,data)
noise(len)=0;
noise(nl:nr) = A111_A602_2007_D_SAC__(nl:nr,1);

%信号S变换
[st_data,st_times,st_frequencies] = st(data);
%信号S谱
contourf(st_times,st_frequencies,abs(st_data));

%噪声S变换
%[st_noise,st_times,st_frequencies] = st(noise);
%噪声S谱
%contourf(st_times,st_frequencies,abs(st_noise));

%取出S变换结果的虚部实部
real_d = real(st_data);
imag_d = imag(st_data);
% real_n = real(st_noise);
% imag_n = imag(st_noise);
real_n = real(st_data(:,nl:nr));
imag_n = imag(st_data(:,nl:nr));
%设定参数
singma_real = std(real_n,1,2);
singma_real = singma_real';
namenda_real = singma_real*sqrt(2*log10(len));
singma_imag = std(imag_n,1,2);
singma_imag = singma_imag';
namenda_imag = singma_imag*sqrt(2*log10(len));

gada =  0.8;
alpha = 0.9;
msd(size(gada,2),size(alpha,2)) = 0;
for k=1:size(gada,2)
gama_real = gada(k)*namenda_real;
gama_imag = gada(k)*namenda_imag;

for i = 1:size(alpha,2)
alpha_real = alpha(i);
alpha_imag = alpha(i);

%定义阈值函数
fc_real = real_d;
fc_imag = imag_d;

for m = 1:size(real_d,1)
    for n = 1:size(real_d,2)
        if abs(real_d(m,n)) >= namenda_real(m)
            fc_real(m,n) = real_d(m,n)-sign(real_d(m,n))*(1-alpha_real)*namenda_real(m);
        elseif abs(real_d(m,n)) <= gama_real(m)
            fc_real(m,n) = 0;
        else
            fc_real(m,n) = alpha_real*namenda_real(m)*((abs(real_d(m,n))-gama_real(m))/(namenda_real(m)-gama_real(m)))^2*((alpha_real-3)*(abs(real_d(m,n))-gama_real(m))/(namenda_real(m)-gama_real(m))+4-alpha_real);
        end
        if abs(imag_d(m,n)) >= namenda_imag(m)
            fc_imag(m,n) = imag_d(m,n)-sign(imag_d(m,n))*(1-alpha_imag)*namenda_imag(m);
        elseif abs(imag_d(m,n)) <= gama_imag(m)
            fc_imag(m,n) = 0;
        else
            fc_imag(m,n) = alpha_imag*namenda_imag(m)*((abs(imag_d(m,n))-gama_imag(m))/(namenda_imag(m)-gama_imag(m)))^2*((alpha_imag-3)*(abs(imag_d(m,n))-gama_imag(m))/(namenda_imag(m)-gama_imag(m))+4-alpha_imag);
        end         
    end
end

fc = fc_real+fc_imag*1i;

contourf(st_times,st_frequencies,abs(fc));
xlabel('走时');ylabel('频率');title('去噪的S谱');axis([0 150 0.0 0.5]);
%对滤波后的信号反变换
[ts_data,ts1,fullstspe] = inverse_st(fc);
for j = nr:len
    msd(k,i) = msd(k,i) +(data(j)-ts_data(j))^2;
end
msd(k,i) = msd(k,i)/(len-nr+1);
end
end
% plot(alpha,msd(1,:),'b',alpha,msd(2,:),'r',alpha,msd(3,:),'m')
% legend('0.2','0.5','0.8');xlabel('α');ylabel('MSD');title('The MSD versus α')
%plot(t,ts_data);
%plot(t,ts_data,'b',t,data,'r');legend('去噪后波形','原波形');xlim([0 150]);
%grid on;            
fid = fopen('ts.asc','w');
fprintf(fid,'%e\n',ts_data);


