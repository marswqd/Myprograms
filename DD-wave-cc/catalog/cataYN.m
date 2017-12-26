clear all;
%%%%%%%%%输入参数部分%%%%%%%%%%%%%%%%%%%%%%%%
%研究区的 纬度范围   经度范围    震级范围
%         小   大   小    大     小  大
%weizhi=[27.0 28.0  103.0 105.0  1.0 8.0]; %彝良
weizhi=[-90.0 90.0  -180.0 180.0  0.0 10.0]; %整个中国
%研究的时间范围,精确到日
date=[2014 1 1;2015 1 1]; %
% 要处理文件的名称
infile=char('YN2014.txt');   %输入地震目录(观测报告)
outfile=char('2014all.txt'); %输出震相部分
%example
%YN 2012/09/01 02:08:33.4  27.846  100.729   6  0.3     3   3 eq 51 四川盐源
%YN YNI   SHZ I D Pg      1.0 V  02:08:35.56  -0.02   11.0 228.1                          
%         SHN     Sg      1.0 V  02:08:37.05   0.06                                       
%         SHN     SMN     1.0 D  02:08:37.47                          49.1   0.22         
%         SHE     SME     1.0 D  02:08:37.80                          42.3   0.18 ML   0.6
%SC LGH   BHZ     Pg      1.0 V  02:08:36.83   0.12   19.0 138.2                          
%         BHN     Sg      1.0 V  02:08:39.12   0.06                                       
%         BHE     SME     1.0 D  02:08:39.82                           7.8   0.25         
%         BHN     SMN     1.0 D  02:08:39.90                          12.6   0.32 ML   0.1
dizhen=char('/');          %地震事件识别符号
id=1400000;                       %事件初始ID
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
fidout=fopen(outfile,'w');
%stain=fopen('stain.txt','w');%地震目录(观测报告)中包含的台站
stano=fopen('stano.txt','w');%不知道经纬度的台站
Pweight=1.0; Sweight=0.5;    %震相权重
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
i=0;k=1;id1=id;
disp('开始判读');
while ~feof(fid)
    txt=fgetl(fid);i=i+1;
    if(i==num(k))%找到地震
        k=k+1;        
        if ~isspace(txt(29:29)) && ~isspace(txt(49:49)) %纬度、震级存在
            evla=str2num(txt(26:32));evlo=str2num(txt(35:41));mag=str2num(txt(48:50));%纬度 经度 震级
		    nian1=str2num(txt(4:7));yue1=str2num(txt(9:10));ri1=str2num(txt(12:13));%发震时刻:年月日 
            t1=[date(1,1) date(1,2) date(1,3) 0 0 0];t2=[date(2,1) date(2,2) date(2,3) 0 0 0];
            t=[nian1 yue1 ri1 0 0 0];b=etime(t,t1);e=etime(t,t2);%计算时间范围
            if(evla>=weizhi(1) && evla<=weizhi(2) && evlo>=weizhi(3) && evlo<=weizhi(4) && mag>=weizhi(5) && mag<=weizhi(6) && b>=0 && e<=0)
                dep=str2num(txt(43:46));
                if isspace(txt(43:46)),dep=10.0;end %震源深度,若无,则设为10.0km
                id=id+1;
                disp('读事件信息')
                disp(txt);                                     
                shi1=str2num(txt(15:16));feng1=str2num(txt(18:19));miao1=str2num(txt(21:24));%发震时刻:时分秒
                fprintf(fidout,'#\t%d\t%d\t%d\t',nian1,yue1,ri1);     %年月日
                fprintf(fidout,'%d\t%d\t%6.3f\t',shi1,feng1,miao1);    %时分秒
                fprintf(fidout,'%7.3f\t%7.3f\t%4.1f\t',evla,evlo,dep);   %震源位置及深度
                fprintf(fidout,'%3.1f\t0\t0\t0\t%d\t',mag,id);       %震级及事件编号      
                p=strfind(txt,'eq');fprintf(fidout,'%s\n',strtrim(txt(p+5:end)));
			    st1='';st2='';
                disp('读台站信息')
                jj=0;
                for j=1:(num(k)-num(k-1)-1)
                    txt=fgetl(fid);i=i+1;
				    if isempty(txt),continue,end %跳过空行
                    %if ~strcmpi(txt(1:3),'DPB'),continue,end %非震相标定,跳过                    
                    st2=st1;
					if ~isspace(txt(4:6))
                        C=textscan(txt,'%s',2);range=char(C{1,1}{1,1});st1=char(C{1,1}{2,1});%disp(st1);disp(range);%台站名 所在地
                        st1=upper(st1); %转化为大写字母
						if(j==1 || (~isempty(st2) && ~strcmpi(st1,st2))) %新的台站
                            jj=jj+1;
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
                            n1(jj)=size(st1,2);jst(jj,1:n1(jj))=st1;
                            n2(jj)=size(range,2);jrange(jj,1:n2(jj))=range;
                            jdist(jj)=dist;jaz(jj)=az;
                            tPg(jj)=-999;tSg(jj)=-999;tPn(jj)=-999;tSn(jj)=-999;
                        end
                    end                  
                    pha=txt(18:19);%震相
%                     if isspace(txt(4:6))
%                         txt(4:6)=st1;txt(1:2)=range;
%                     end                   
                    %st1=txt(4:6);pha=txt(18:19);range=txt(1:2);%台站名 震相 所在地
                    %%%%%%%%%%%%%%选取台站%%%%%%%%%%%%%%																		                   
                    if(strcmpi(pha,'Pg') || strcmpi(pha,'Sg') || strcmpi(pha,'Pn') || strcmpi(pha,'Sn'))                          
						fprintf('选取台站 %s\n',st1);
						disp(txt);    
						%nian2=str2num(txt(35:38));yue2=str2num(txt(40:41));ri2=str2num(txt(43:44));  %震相到时:年月日
						shi2=str2num(txt(33:34));feng2=str2num(txt(36:37));miao2=str2num(txt(39:43));%震相到时:时分秒
						shicha=(shi2-shi1)*60*60+(feng2-feng1)*60+(miao2-miao1); %走时s
                        if(shicha>-86400 && shicha<-85000),shicha=shicha+86400;end
                        if(shicha<0),continue;end
                        if(strcmpi(pha,'Pg'))
							tPg(jj)=shicha;cPg(jj,1:3)=txt(10:12);
						elseif(strcmpi(pha,'Sg'))
							tSg(jj)=shicha;cSg(jj,1:3)=txt(10:12);
						elseif(strcmpi(pha,'Pn'))
							tPn(jj)=shicha;cPn(jj,1:3)=txt(10:12);
						elseif(strcmpi(pha,'Sn'))
							tSn(jj)=shicha;cSn(jj,1:3)=txt(10:12);
						end
                    end
                end
                for j=1:jj                        
                    if cst==1
						for m=1:nst
                            if(strcmpi(jst(j,:),st(m,:)))
                                if((tPg(j)~=-999 && tPn(j)~=-999 && tPn(j) < tPg(j)) || (tPg(j)==-999 && tPn(j)~=-999))
                                    fprintf(fidout,'%s\t%8.3f\t%5.3f\t%s\t%8.3f\t%6.2f\t%s\t%s\t%s\n',jst(j,1:n1(j)),tPn(j),Pweight,'P',...
                                    jdist(j),jaz(j),jrange(j,1:n2(j)),cPn(j,1:3),'Pn');
                                elseif((tPg(j)~=-999 && tPn(j)~=-999 && tPn(j) >= tPg(j)) || (tPg(j)~=-999 && tPn(j)==-999))
                                    fprintf(fidout,'%s\t%8.3f\t%5.3f\t%s\t%8.3f\t%6.2f\t%s\t%s\t%s\n',jst(j,1:n1(j)),tPg(j),Pweight,'P',...
                                    jdist(j),jaz(j),jrange(j,1:n2(j)),cPg(j,1:3),'Pg');
                                end
                                if((tSg(j)~=-999 && tSn(j)~=-999 && tSn(j) < tSg(j)) || (tSg(j)==-999 && tSn(j)~=-999))
                                    fprintf(fidout,'%s\t%8.3f\t%5.3f\t%s\t%8.3f\t%6.2f\t%s\t%s\t%s\n',jst(j,1:n1(j)),tSn(j),Sweight,'S',...
                                    jdist(j),jaz(j),jrange(j,1:n2(j)),cSn(j,1:3),'Sn');
                                elseif((tSg(j)~=-999 && tSn(j)~=-999 && tSn(j) >= tSg(j)) || (tSg(j)~=-999 && tSn(j)==-999))
                                    fprintf(fidout,'%s\t%8.3f\t%5.3f\t%s\t%8.3f\t%6.2f\t%s\t%s\t%s\n',jst(j,1:n1(j)),tSg(j),Sweight,'S',...
                                    jdist(j),jaz(j),jrange(j,1:n2(j)),cSg(j,1:3),'Sg');
                                end
                                
%                               if(tPg(j)~=-999)
%                                   fprintf(fidout,'%s\t%8.3f\t%5.3f\t%s\t%8.3f\t%6.2f\t%s\t%s\t\n',jst(j,1:n1(j)),tPg(j),Pweight,'P',...
%                                   jdist(j),jaz(j),jrange(j,1:n2(j)),cPg(j,1:3));
%                               end
%                               if(tSg(j)~=-999)
%                                   fprintf(fidout,'%s\t%8.3f\t%5.3f\t%s\t%8.3f\t%6.2f\t%s\t%s\t\n',jst(j,1:n1(j)),tSg(j),Sweight,'S',...
%                                   jdist(j),jaz(j),jrange(j,1:n2(j)),cSg(j,1:3));
%                               end                                 
                                
                            end                          															
                        end
                    else
                        if((tPg(j)~=-999 && tPn(j)~=-999 && tPn(j) < tPg(j)) || (tPg(j)==-999 && tPn(j)~=-999))
                            fprintf(fidout,'%s\t%8.3f\t%5.3f\t%s\t%8.3f\t%6.2f\t%s\t%s\t%s\n',jst(j,1:n1(j)),tPn(j),Pweight,'P',...
                            jdist(j),jaz(j),jrange(j,1:n2(j)),cPn(j,1:3),'Pn');
                        elseif((tPg(j)~=-999 && tPn(j)~=-999 && tPn(j) >= tPg(j)) || (tPg(j)~=-999 && tPn(j)==-999))
                            fprintf(fidout,'%s\t%8.3f\t%5.3f\t%s\t%8.3f\t%6.2f\t%s\t%s\t%s\n',jst(j,1:n1(j)),tPg(j),Pweight,'P',...
                            jdist(j),jaz(j),jrange(j,1:n2(j)),cPg(j,1:3),'Pg');
                        end
                        if((tSg(j)~=-999 && tSn(j)~=-999 && tSn(j) < tSg(j)) || (tSg(j)==-999 && tSn(j)~=-999))
                            fprintf(fidout,'%s\t%8.3f\t%5.3f\t%s\t%8.3f\t%6.2f\t%s\t%s\t%s\n',jst(j,1:n1(j)),tSn(j),Sweight,'S',...
                            jdist(j),jaz(j),jrange(j,1:n2(j)),cSn(j,1:3),'Sn');
                        elseif((tSg(j)~=-999 && tSn(j)~=-999 && tSn(j) >= tSg(j)) || (tSg(j)~=-999 && tSn(j)==-999))
                            fprintf(fidout,'%s\t%8.3f\t%5.3f\t%s\t%8.3f\t%6.2f\t%s\t%s\t%s\n',jst(j,1:n1(j)),tSg(j),Sweight,'S',...
                            jdist(j),jaz(j),jrange(j,1:n2(j)),cSg(j,1:3),'Sg');
                        end
                        
%                         if(tPg(j)~=-999)
%                             fprintf(fidout,'%s\t%8.3f\t%5.3f\t%s\t%8.3f\t%6.2f\t%s\t%s\t\n',jst(j,1:n1(j)),tPg(j),Pweight,'P',...
%                             jdist(j),jaz(j),jrange(j,1:n2(j)),cPg(j,1:3));
%                         end
%                         if(tSg(j)~=-999)
%                             fprintf(fidout,'%s\t%8.3f\t%5.3f\t%s\t%8.3f\t%6.2f\t%s\t%s\t\n',jst(j,1:n1(j)),tSg(j),Sweight,'S',...
%                             jdist(j),jaz(j),jrange(j,1:n2(j)),cSg(j,1:3));
%                         end 
                        
                    end                    
                end
            end              
        end
    end
end
id2=id;
fclose(fid);fclose(fidout);fclose(stano);
fprintf('\nThere are %i events in the file %s, %i events are slected in the file %s.\n',neve,infile,id2-id1,outfile);
