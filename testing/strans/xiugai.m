%对信号赋值
minfreq=0.5;
midfreq=1;
maxfreq=3;
len=200;
t=0.1:0.1:20;
%定义三个信号数组
 signal_1(200)=0;
 signal_2(200)=0;
 signal_3(200)=0;
%生成信号
signal_1(142:192)=sin(2*pi*minfreq*t(142:192));
signal_2(132:182)=sin(2*pi*midfreq*t(132:182)); 
signal_3(132:162)=sin(2*pi*maxfreq*t(132:162)); 
%信号的合成
signal=signal_1+signal_2+signal_3;
%对信号S变换
[st_matrix_signal,st_times,st_frequencies] = st(signal);
%对信号加随机噪声20%并s变换
signal_noise=signal+(rand(1,200)-0.5).*2*0.1;
[st_signal_noise,st_times,st_frequencies] = st(signal_noise);
%信号与噪声信号S谱的差值谱
%noise=signal_noise-signal;
st_matrix_noise=st_signal_noise-st_matrix_signal;

	%取出S变换结果的虚部实部
	real_s_signal=real(st_signal_noise);
	imag_s_signal=imag(st_signal_noise);
	%定义阈值函数
	filter_real(101,200)=0;
    filter_imag(101,200)=0;
    % 为方便程序中书写将信号—噪声的虚实部赋值给x,y数组 
    x=real_s_signal;
    y=imag_s_signal;
    %取出噪声谱中的虚部和实部
    real_noise=real(st_matrix_noise);
    imag_noise=imag(st_matrix_noise);
    
%先考虑实部的滤波函数
for m=1:101
    %求噪声方差
	sum=0.0;
	for u=1:132,
		sum=sum+real_noise(m,u);
	end
	average=sum/132;  %噪声的平均值
	singma2=0.0;
	for u=1:132,
		singma2=singma2+(real_noise(m,u)-average).^2;
	end
	singma=sqrt(singma2/132);   %噪声的方差
	%定义参数namenda，gama，alpha
	namenda=singma*sqrt(2*log10(len));
	gama=0.5*namenda;
	alpha=0.7;
	namenda=namenda/10;
	gama=gama/10;
	
    for n=1:200
       if abs(x(m,n))>=namenda
           filter_real(m,n)=x(m,n)-sign(x(m,n))*(1-alpha)*namenda;
         else if abs(x(m,n))<=gama
           filter_real(m,n)=0;
       else
           filter_real(m,n)=alpha*namenda*((abs(x(m,n))-gama)/(namenda-gama))^2*((alpha-3)*((abs(x(m,n))-gama)/(namenda-gama))+4-alpha);
         end
       end
    end
end

%在考虑虚部的滤波函数
for m=1:101
    %求噪声方差
	sum=0.0;
	for u=1:132,
		sum=sum+imag_noise(m,u);
	end
	average=sum/132;  %噪声的平均值
	singma2=0.0;
	for u=1:132,
		singma2=singma2+(imag_noise(m,u)-average).^2;
	end
	singma=sqrt(singma2/132);   %噪声的方差
	%定义参数namenda，gama，alpha
	namenda=singma*sqrt(2*log10(len));
	gama=0.5*namenda;
	alpha=0.7;
	namenda=namenda/10;
	gama=gama/10;
	
    for n=1:200
       if abs(y(m,n))>=namenda
           filter_imag(m,n)=y(m,n)-sign(y(m,n))*(1-alpha)*namenda;
         else if abs(y(m,n))<=gama
           filter_imag(m,n)=0;
       else
           filter_imag(m,n)=alpha*namenda*((abs(y(m,n))-gama)/(namenda-gama))^2*((alpha-3)*((abs(y(m,n))-gama)/(namenda-gama))+4-alpha);
         end
       end
    end
end
%对S变换矩阵st_matrix滤波组合
filter_st_matrix=filter_real+filter_imag*i;
%filter_st_matrix=filter_real.*real_s_signal+filter_imag.*imag_s_signal*i;
contourf(st_times./10,st_frequencies*10,abs(filter_st_matrix));
%对滤波后的信号反变换
[ts,ts1,fullstspe] = inverse_st(filter_st_matrix);
plot(t,ts);
grid on;
