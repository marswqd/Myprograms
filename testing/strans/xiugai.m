%���źŸ�ֵ
minfreq=0.5;
midfreq=1;
maxfreq=3;
len=200;
t=0.1:0.1:20;
%���������ź�����
 signal_1(200)=0;
 signal_2(200)=0;
 signal_3(200)=0;
%�����ź�
signal_1(142:192)=sin(2*pi*minfreq*t(142:192));
signal_2(132:182)=sin(2*pi*midfreq*t(132:182)); 
signal_3(132:162)=sin(2*pi*maxfreq*t(132:162)); 
%�źŵĺϳ�
signal=signal_1+signal_2+signal_3;
%���ź�S�任
[st_matrix_signal,st_times,st_frequencies] = st(signal);
%���źż��������20%��s�任
signal_noise=signal+(rand(1,200)-0.5).*2*0.1;
[st_signal_noise,st_times,st_frequencies] = st(signal_noise);
%�ź��������ź�S�׵Ĳ�ֵ��
%noise=signal_noise-signal;
st_matrix_noise=st_signal_noise-st_matrix_signal;

	%ȡ��S�任������鲿ʵ��
	real_s_signal=real(st_signal_noise);
	imag_s_signal=imag(st_signal_noise);
	%������ֵ����
	filter_real(101,200)=0;
    filter_imag(101,200)=0;
    % Ϊ�����������д���źš���������ʵ����ֵ��x,y���� 
    x=real_s_signal;
    y=imag_s_signal;
    %ȡ���������е��鲿��ʵ��
    real_noise=real(st_matrix_noise);
    imag_noise=imag(st_matrix_noise);
    
%�ȿ���ʵ�����˲�����
for m=1:101
    %����������
	sum=0.0;
	for u=1:132,
		sum=sum+real_noise(m,u);
	end
	average=sum/132;  %������ƽ��ֵ
	singma2=0.0;
	for u=1:132,
		singma2=singma2+(real_noise(m,u)-average).^2;
	end
	singma=sqrt(singma2/132);   %�����ķ���
	%�������namenda��gama��alpha
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

%�ڿ����鲿���˲�����
for m=1:101
    %����������
	sum=0.0;
	for u=1:132,
		sum=sum+imag_noise(m,u);
	end
	average=sum/132;  %������ƽ��ֵ
	singma2=0.0;
	for u=1:132,
		singma2=singma2+(imag_noise(m,u)-average).^2;
	end
	singma=sqrt(singma2/132);   %�����ķ���
	%�������namenda��gama��alpha
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
%��S�任����st_matrix�˲����
filter_st_matrix=filter_real+filter_imag*i;
%filter_st_matrix=filter_real.*real_s_signal+filter_imag.*imag_s_signal*i;
contourf(st_times./10,st_frequencies*10,abs(filter_st_matrix));
%���˲�����źŷ��任
[ts,ts1,fullstspe] = inverse_st(filter_st_matrix);
plot(t,ts);
grid on;
