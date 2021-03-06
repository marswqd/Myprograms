%生成高斯分布的随机误差

clear all;
infile=char('dt.cc'); %双差数据文件
out=char('gserr.txt');
num=100;  %重采样次数
eband=0.01  ; %标准差
epair=char('#');

fid=fopen(infile,'rt');

disp('Step 1')
i=1;j=1;k=1;
while ~feof(fid) 
    txt=fgetl(fid);   
    if ~isempty(txt)   %判断是否为空行  
        e=strfind(txt,epair);%p=strfind(txt,'P');s=strfind(txt,'S');        
        if isempty(e)      
            i=i+1;
        else
            j=j+1;  %第j个事件对位于第k行
        end
        k=k+1;
    end   
end
ndt=i-1;nep=j-1;  
frewind(fid);

disp('Step 2');%生成随机噪声数组ndt*num
%ndt=1000;num=200;
err=zeros(ndt,num);
for i=1:ndt
    err(i,:)=normrnd(0,eband,1,num);%生成高斯分布的随机误差，均值为0，标准差为eband
end
% for i=1:ndt
%     rr=rand(1,num);%生成均匀分布的随机误差
%     err(i,:)=(rr*2-1).*eband;
% end
% for i=1:num
%     err(:,i)=sort(randi(ndt,[ndt 1]));%生成均匀分布的随机整数并从小到大排序
% end

%std=std(dt,1);stdc=std(cdt,1);std2=std(dt-cdt,1);wdt=dt.*w;wstd=std(wdt,1);
%mad=median(abs(dt-median(dt)));madc=median(abs(cdt-median(cdt)));
%eband=std0;

save(out,'err','-ascii');

% disp('Step 3');
% uw=1.0;
% for i=1:num
%     fprintf('%s\t%d\n','loop:',i);
%     wname=sprintf('%s%03d',out,i);
%     r=err(:,i);newdt=dt+r';
%     fid=fopen(wname,'w');
%     for j=1:nep-1
%         fprintf(fid,'%s\n',deblank(ep(j,:)));
%         for k=eline(j)-(j-1):eline(j+1)-(j+1)
%             a=sta(k,:);b=pha(k,:);
%             %fprintf(fid,'%s   %f   %f   %s   %d\n',deblank(a),newdt(k),w(k),deblank(b),idx(k));
%             fprintf(fid,'%s   %f   %f   %s   %d\n',deblank(a),newdt(k),uw,deblank(b),idx(k));  %数据加随机误差+单位权重
%         end       
%     end
%     fprintf(fid,'%s\n',deblank(ep(nep,:)));
%     for k=eline(nep)-(nep-1):ndt
%         a=sta(k,:);b=pha(k,:);
%         %fprintf(fid,'%s   %f   %f   %s   %d\n',deblank(a),newdt(k),w(k),deblank(b),idx(k));
%         fprintf(fid,'%s   %f   %f   %s   %d\n',deblank(a),newdt(k),uw,deblank(b),idx(k));  %数据加随机误差+单位权重
%     end
%     fclose(fid);
% end

% ndt=200;eband=0.027;
% r=rand(1,ndt);err=(r*2-1).*eband;%生成均匀分布的随机误差
% r2=normrnd(0,eband,1,ndt);%生成均匀分布的随机误差
% hist(r)
% hist(r2,50)
% hold on
% gb=max(abs(r2));
% x=-gb:gb/100:gb;
% gs=normpdf(x,0,eband]);
% plot(x,gs)
    





