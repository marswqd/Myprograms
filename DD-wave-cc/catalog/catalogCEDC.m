clear all;
%%%%%%%%%输入参数部分%%%%%%%%%%%%%%%%%%%%%%%%
%研究区的 纬度范围   经度范围    震级范围
%         小   大    小    大     小  大
weizhi=[23.2 23.6  100.3 100.7  0.0 8.0]; %景谷
%研究的时间范围,精确到日
date=[2014 9 1;2016 12 31];
%选取台站的名称,不区分大小写
st=char('BAS','BIJ','BJT','CAD','CHT','CAY','CQT','CUX','CYU','CZS','CUX','DAY','DOC','EYA','FUN','GEJ','GOS','GYA',...
        'HEQ','HLT','HUP','HWS','JIG','JIH','JIP','JIS','JLI','KMI','LPS','LAB','LAC','LAK','LAP','LBO','LIC',...
		'LIJ','LOP','LIZ','LUQ','LUS','LGH','MAL','MAS','MBI','MEK','MIL','MLP','MEL','MLA','MZT','MIZ','NNS',...
		'PZH','PGE','PXS','PYZ','QIJ','QKT','RST','RHT','SBT','SIM','TOH','TUS','TNC','WAD','WES','WNT','XBT',...
		'XUW','XHT','XCE','YAJ','YIM','YOD','YOS','YUJ','YUL','YUM','YUX','YNI','YYU','ZAT','ZOD','YIL','PAH','LOJ');
nst=size(st,1); %台站数
cst=0; %是否只选择st中的台站，cst=1表示只选择st中的台站，cst=else表士选择地震目录中的所有台站
stall=importdata('sta2014.txt',' '); %读入台站经纬度数据，用于计算震中距
nstall=size(stall.data,1);
%example
%BAS   25.118000       99.146599    
%BJT   27.243500       105.34970 
% 要处理文件的名称
infile=char('CEDC-JG.txt');   %输入地震目录(观测报告)
outfile=char('CEDC-JG-phase.txt'); %输出震相部分
fidout=fopen(outfile,'w');
%stain=fopen('stain.txt','w');%地震目录(观测报告)中包含的台站
stano=fopen('stano.txt','w');%不知道经纬度的台站
Pweight=1.0; Sweight=0.5;    %震相权重
dizhen=char('DBO');          %地震事件识别符号
%example
%DBO SC 2012-09-01 03:34:31.67  28.130  101.160   9 ML    1.8  0.410 1   0  10 51 51 eq 四川木里            
%DEO C SC.201209010334.0001                      0  1.0 M                 293.4   3.0  12.2      17  17                                                                      
%DMB Ml     1.8        10
%DPB SC   JLO BHZ          Pg 1  V 2012-09-01 03:34:50.10    0.78  102.2 199.7                            
%DPB SC   JLO BHN         SMN 1 SD 2012-09-01 03:35:02.95          102.2             0.0   0.12 Ml     1.6
%DPB SC   JLO BHE          Sg 1  V 2012-09-01 03:35:01.59   -0.43  102.2 199.7                            
%DPB SC   JLO BHE         SME 1 SD 2012-09-01 03:35:02.78          102.2             0.0   0.14 Ml     1.6  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%计算部分
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fid=fopen(infile,'rt');
i=1;k=1;
while ~feof(fid) 
    txt=fgetl(fid);p=-1;   
    if ~isempty(txt)   %判断是否为空行  
        p=strfind(txt,dizhen);
        if p~=-1       
            num(k)=i;     %第k个事件位于第i行
            k=k+1;
        end
        %if strcmpi(txt(1:3),'DPB')    
            %fprintf(stain,'%s\n',txt(10:12));
        %end   
    end
    i=i+1; 
end
neve=k-1;num(k)=i;   %num的最后一个元素代表infile的行数
fclose(fid);

fid=fopen(infile,'rt');
i=0;k=1;id=0;id1=id;
disp('开始判读');
while ~feof(fid)
    txt=fgetl(fid);i=i+1;
    if(i==num(k))%找到地震
        k=k+1;
        if ~isspace(txt(37:37)) && ~isspace(txt(59:59)) %纬度、震级存在
            %[HBO] = ...
            %textscan(txt,'%s %s %s %s %f %f %f %s %f');
           % evla=Epi_lat;evlo=Epi_lon;mag=Mag_value;%纬度 经度 震级
		   % [nian1,yue1,ri1]=textscan(Epi_date,'%d %d %d','Delimiyer','-');%发震时刻:年月日                       
           % [shi1,feng1,miao1]=textscan(Epi_time,'%d %d %f','Delimiyer',':');%发震时刻:时分秒
            evla=str2num(txt(35:39));evlo=str2num(txt(42:47));mag=str2num(txt(58:60));dep=str2num(txt(49:51));%纬度 经度 震级 深度
		    b=strfind(txt,'-');if isempty(b),continue,end;a=b(1)-4;
			nian1=str2num(txt(a:a+3));yue1=str2num(txt(a+5:a+6));ri1=str2num(txt(a+8:a+9));%发震时刻:年月日 
			b=strfind(txt,':');if isempty(b),continue,end;a=b(1)-2;
			shi1=str2num(txt(a:a+1));feng1=str2num(txt(a+3:a+4));miao1=str2num(txt(a+6:a+10));%发震时刻:时分秒
            t1=[date(1,1) date(1,2) date(1,3) 0 0 0];t2=[date(2,1) date(2,2) date(2,3) 0 0 0];
            t=[nian1 yue1 ri1 0 0 0];b=etime(t,t1);e=etime(t,t2);%计算时间范围
            if(evla>=weizhi(1) && evla<=weizhi(2) && evlo>=weizhi(3) && evlo<=weizhi(4) && mag>=weizhi(5) && mag<=weizhi(6) && b>=0 && e<=0)
                id=id+1;
                disp('读事件信息')
                disp(txt);                                     
                %[shi1,feng1,miao1]=textscan(Epi_time,'%d %d %f','Delimiyer',':');%发震时刻:时分秒
                %shi1=str2num(txt(19:20));feng1=str2num(txt(22:23));miao1=str2num(txt(25:29));%发震时刻:时分秒
                fprintf(fidout,'#\t%d\t%d\t%d\t',nian1,yue1,ri1);     %年月日
                fprintf(fidout,'%d\t%d\t%5.2f\t',shi1,feng1,miao1);    %时分秒
                fprintf(fidout,'%7.3f\t%7.3f\t%4.1f\t',evla,evlo,dep);   %震源位置及深度
                fprintf(fidout,'%3.1f\t0\t0\t0\t%d\t',mag,id);       %震级及事件编号      
                p=strfind(txt,'eq');fprintf(fidout,'%s\n',strtrim(txt(p+2:end)));
			    st1='';
                disp('读台站信息')
                for j=1:(num(k)-num(k-1)-1)
                    txt=fgetl(fid);i=i+1;
				    if isempty(txt),continue,end %跳过空行
                    if ~strcmpi(txt(1:3),'DPB'),continue,end %非震相标定,跳过
                    if isspace(txt(10:12))
                        txt(10:14)=st1;
                    end
                    %[HPB,Net_code,Sta_code,Chn_code,Phase_name,Weight,Rec_type,Phase_date,Phase_time] = ...
                    %textscan(txt,'%s %s %s %s %s %d %s %s %s ');                    
                    st1=strtrim(txt(10:14));pha=strtrim(txt(30:33));%台站名 震相
                    net=txt(6:7);chn=strtrim(txt(15:19));%台网名 分量
					b=strfind(txt,'-');if isempty(b),continue,end;a=b(1)-4;
					nian2=str2num(txt(a:a+3));yue2=str2num(txt(a+5:a+6));ri2=str2num(txt(a+8:a+9));  %震相到时:年月日
					b=strfind(txt,':');if isempty(b),continue,end;a=b(1)-2;
					shi2=str2num(txt(a:a+1));feng2=str2num(txt(a+3:a+4));miao2=str2num(txt(a+6:a+10));%震相到时:时分秒                    
                    %%%%%%%%%%%%%%选取台站%%%%%%%%%%%%%%					
					if cst==1
						for m=1:nst
							if(strcmpi(st1,st(m,:)) && (strcmpi(pha,'Pg') || strcmpi(pha,'Sg')))                          
								fprintf('选取台站 %s\n',st1);
								disp(txt);    
								shicha=(shi2-shi1)*60*60+(feng2-feng1)*60+(miao2-miao1); %走时s
								dist=0;az=0;
								for n=1:nstall
									if strcmpi(st1,stall.textdata(n))
										stla=stall.data(n,1);stlo=stall.data(n,2); %台站经纬度
										[dist,az]=distance(evla,evlo,stla,stlo);dist=dist*111.195; %计算震中距和方位角
										break;
									end
								end
								if dist==0
									fprintf(stano,'%s\n',st1);
								end
								if(strcmpi(pha,'Pg'))
									fprintf(fidout,'%s\t%8.3f\t%5.3f\t%s\t%8.3f\t%6.2f\t%s\t%s\t\n',st1,shicha,Pweight,'P',dist,az,net,chn);
								else
									fprintf(fidout,'%s\t%8.3f\t%5.3f\t%s\t%8.3f\t%6.2f\t%s\t%s\t\n',st1,shicha,Sweight,'S',dist,az,net,chn);
                                end
                            end
                        end
                    else
                        if(strcmpi(pha,'Pg') || strcmpi(pha,'Sg'))                          
                            fprintf('选取台站 %s\n',st1);
							disp(txt);    
							shicha=(shi2-shi1)*60*60+(feng2-feng1)*60+(miao2-miao1); %走时s
							dist=0;az=0;
							for n=1:nstall
								if strcmpi(st1,stall.textdata(n))
									stla=stall.data(n,1);stlo=stall.data(n,2); %台站经纬度
									[dist,az]=distance(evla,evlo,stla,stlo);dist=dist*111.195; %计算震中距和方位角
									break;
								end
							end
							if dist==0
								fprintf(stano,'%s\n',st1);
							end
							if(strcmpi(pha,'Pg'))
								fprintf(fidout,'%s\t%8.3f\t%5.3f\t%s\t%8.3f\t%6.2f\t%s\t%s\t\n',st1,shicha,Pweight,'P',dist,az,net,chn);
							else
								fprintf(fidout,'%s\t%8.3f\t%5.3f\t%s\t%8.3f\t%6.2f\t%s\t%s\t\n',st1,shicha,Sweight,'S',dist,az,net,chn);
                            end							
                        end
                    end
                end
            end              
        end
    end
end
id2=id;  
fclose(fid);fclose(fidout);fclose(stano);
fprintf('\nThere are %i events in the file %s, %i events are slected in the file %s.\n',neve,infile,id2-id1,outfile);

