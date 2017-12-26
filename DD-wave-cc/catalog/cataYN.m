clear all;
%%%%%%%%%�����������%%%%%%%%%%%%%%%%%%%%%%%%
%�о����� γ�ȷ�Χ   ���ȷ�Χ    �𼶷�Χ
%         С   ��   С    ��     С  ��
%weizhi=[27.0 28.0  103.0 105.0  1.0 8.0]; %����
weizhi=[-90.0 90.0  -180.0 180.0  0.0 10.0]; %�����й�
%�о���ʱ�䷶Χ,��ȷ����
date=[2014 1 1;2015 1 1]; %
% Ҫ�����ļ�������
infile=char('YN2014.txt');   %�������Ŀ¼(�۲ⱨ��)
outfile=char('2014all.txt'); %������ಿ��
%example
%YN 2012/09/01 02:08:33.4  27.846  100.729   6  0.3     3   3 eq 51 �Ĵ���Դ
%YN YNI   SHZ I D Pg      1.0 V  02:08:35.56  -0.02   11.0 228.1                          
%         SHN     Sg      1.0 V  02:08:37.05   0.06                                       
%         SHN     SMN     1.0 D  02:08:37.47                          49.1   0.22         
%         SHE     SME     1.0 D  02:08:37.80                          42.3   0.18 ML   0.6
%SC LGH   BHZ     Pg      1.0 V  02:08:36.83   0.12   19.0 138.2                          
%         BHN     Sg      1.0 V  02:08:39.12   0.06                                       
%         BHE     SME     1.0 D  02:08:39.82                           7.8   0.25         
%         BHN     SMN     1.0 D  02:08:39.90                          12.6   0.32 ML   0.1
dizhen=char('/');          %�����¼�ʶ�����
id=1400000;                       %�¼���ʼID
%ѡȡ̨վ������,�����ִ�Сд
st=char('BAS','BIJ','BJT','CAD','CHT','CAY','CQT','CUX','CYU','CZS','CUX','DAY','DOC','EYA','FUN','GEJ','GOS','GYA',...
        'HEQ','HLT','HUP','HWS','JIG','JIH','JIP','JIS','JLI','KMI','LPS','LAB','LAC','LAK','LAP','LBO','LIC',...
		'LIJ','LOP','LIZ','LUQ','LUS','LGH','MAL','MAS','MBI','MEK','MIL','MLP','MEL','MLA','MZT','MIZ','NNS',...
		'PZH','PGE','PXS','PYZ','QIJ','QKT','RST','RHT','SBT','SIM','TOH','TUS','TNC','WAD','WES','WNT','XBT',...
		'XUW','XHT','XCE','YAJ','YIM','YOD','YOS','YUJ','YUL','YUM','YUX','YNI','YYU','ZAT','ZOD','YIL','PAH','LOJ');
nst=size(st,1); %̨վ��
cst=0; %�Ƿ�ֻѡ��st�е�̨վ��cst=1��ʾֻѡ��st�е�̨վ��cst=else��ʿѡ�����Ŀ¼�е�����̨վ
stall=importdata('sta2014.txt',' '); %����̨վ��γ�����ݣ����ڼ������о�
nstall=size(stall.data,1);
%example
%BAS   25.118000       99.146599    
%BJT   27.243500       105.34970 
fidout=fopen(outfile,'w');
%stain=fopen('stain.txt','w');%����Ŀ¼(�۲ⱨ��)�а�����̨վ
stano=fopen('stano.txt','w');%��֪����γ�ȵ�̨վ
Pweight=1.0; Sweight=0.5;    %����Ȩ��
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%���㲿��
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fid=fopen(infile,'rt');
i=1;k=1;
while ~feof(fid) 
    txt=fgetl(fid);p=-1;   
    if ~isempty(txt)   %�ж��Ƿ�Ϊ����  
        p=strfind(txt,dizhen);
        if p~=-1       
            num(k)=i;     %��k���¼�λ�ڵ�i��
            k=k+1;
        end
        %if strcmpi(txt(1:3),'DPB')    
            %fprintf(stain,'%s\n',txt(10:12));
        %end   
    end
    i=i+1; 
end
neve=k-1;num(k)=i;   %num�����һ��Ԫ�ش���infile������
fclose(fid);

fid=fopen(infile,'rt');
i=0;k=1;id1=id;
disp('��ʼ�ж�');
while ~feof(fid)
    txt=fgetl(fid);i=i+1;
    if(i==num(k))%�ҵ�����
        k=k+1;        
        if ~isspace(txt(29:29)) && ~isspace(txt(49:49)) %γ�ȡ��𼶴���
            evla=str2num(txt(26:32));evlo=str2num(txt(35:41));mag=str2num(txt(48:50));%γ�� ���� ��
		    nian1=str2num(txt(4:7));yue1=str2num(txt(9:10));ri1=str2num(txt(12:13));%����ʱ��:������ 
            t1=[date(1,1) date(1,2) date(1,3) 0 0 0];t2=[date(2,1) date(2,2) date(2,3) 0 0 0];
            t=[nian1 yue1 ri1 0 0 0];b=etime(t,t1);e=etime(t,t2);%����ʱ�䷶Χ
            if(evla>=weizhi(1) && evla<=weizhi(2) && evlo>=weizhi(3) && evlo<=weizhi(4) && mag>=weizhi(5) && mag<=weizhi(6) && b>=0 && e<=0)
                dep=str2num(txt(43:46));
                if isspace(txt(43:46)),dep=10.0;end %��Դ���,����,����Ϊ10.0km
                id=id+1;
                disp('���¼���Ϣ')
                disp(txt);                                     
                shi1=str2num(txt(15:16));feng1=str2num(txt(18:19));miao1=str2num(txt(21:24));%����ʱ��:ʱ����
                fprintf(fidout,'#\t%d\t%d\t%d\t',nian1,yue1,ri1);     %������
                fprintf(fidout,'%d\t%d\t%6.3f\t',shi1,feng1,miao1);    %ʱ����
                fprintf(fidout,'%7.3f\t%7.3f\t%4.1f\t',evla,evlo,dep);   %��Դλ�ü����
                fprintf(fidout,'%3.1f\t0\t0\t0\t%d\t',mag,id);       %�𼶼��¼����      
                p=strfind(txt,'eq');fprintf(fidout,'%s\n',strtrim(txt(p+5:end)));
			    st1='';st2='';
                disp('��̨վ��Ϣ')
                jj=0;
                for j=1:(num(k)-num(k-1)-1)
                    txt=fgetl(fid);i=i+1;
				    if isempty(txt),continue,end %��������
                    %if ~strcmpi(txt(1:3),'DPB'),continue,end %������궨,����                    
                    st2=st1;
					if ~isspace(txt(4:6))
                        C=textscan(txt,'%s',2);range=char(C{1,1}{1,1});st1=char(C{1,1}{2,1});%disp(st1);disp(range);%̨վ�� ���ڵ�
                        st1=upper(st1); %ת��Ϊ��д��ĸ
						if(j==1 || (~isempty(st2) && ~strcmpi(st1,st2))) %�µ�̨վ
                            jj=jj+1;
                            dist=0;az=0;
                            for n=1:nstall
                                if strcmpi(st1,stall.textdata(n))
                                    stla=stall.data(n,1);stlo=stall.data(n,2); %̨վ��γ��
                                    [dist,az]=distance(evla,evlo,stla,stlo);dist=dist*111.195; %�������о�ͷ�λ��
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
                    pha=txt(18:19);%����
%                     if isspace(txt(4:6))
%                         txt(4:6)=st1;txt(1:2)=range;
%                     end                   
                    %st1=txt(4:6);pha=txt(18:19);range=txt(1:2);%̨վ�� ���� ���ڵ�
                    %%%%%%%%%%%%%%ѡȡ̨վ%%%%%%%%%%%%%%																		                   
                    if(strcmpi(pha,'Pg') || strcmpi(pha,'Sg') || strcmpi(pha,'Pn') || strcmpi(pha,'Sn'))                          
						fprintf('ѡȡ̨վ %s\n',st1);
						disp(txt);    
						%nian2=str2num(txt(35:38));yue2=str2num(txt(40:41));ri2=str2num(txt(43:44));  %���ൽʱ:������
						shi2=str2num(txt(33:34));feng2=str2num(txt(36:37));miao2=str2num(txt(39:43));%���ൽʱ:ʱ����
						shicha=(shi2-shi1)*60*60+(feng2-feng1)*60+(miao2-miao1); %��ʱs
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
