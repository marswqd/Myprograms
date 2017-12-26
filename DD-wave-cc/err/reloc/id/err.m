clear all;
mintimes=80; %一个事件的最小的定位次数
outfile='err.dat'; %事件误差、相关性文件
logfile='err.log'; %输出文件
t0=[ 2009 1 1 0 0 0 ]; %参考时间

idtxt=dir('id_*.txt');ntxt=size(idtxt,1);
fid=fopen(outfile,'w');
for i=1:ntxt
    txt=load(idtxt(i).name);m=size(txt,1);
    if m<mintimes
        continue
    end
    id=txt(1,1);lat=txt(:,2);lon=txt(:,3);dep=txt(:,4);
    year=txt(:,11);month=txt(:,12);day=txt(:,13);hour=txt(:,14);minute=txt(:,15);sec=txt(:,16);
    t=[ year month day hour minute sec ];t1=zeros(m,6);
    for j=1:m
        t1(j,1:6)=t0;
    end
    O=etime(t,t1);
    s_lat=std(lat,1)*111.195*2;s_lon=std(lon,1)*111.195*2;s_dep=std(dep,1)*2;s_O=std(O,1)*2;
    a=corrcoef(lat,lon);a(isnan(a))=0;c_latlon=a(1,2);
    a=corrcoef(lat,dep);a(isnan(a))=0;c_latdep=a(1,2);
    a=corrcoef(lon,dep);a(isnan(a))=0;c_londep=a(1,2);
    a=corrcoef(lat,O);  a(isnan(a))=0;c_latO=a(1,2);
    a=corrcoef(lon,O);  a(isnan(a))=0;c_lonO=a(1,2);
    a=corrcoef(dep,O);  a(isnan(a))=0;c_depO=a(1,2);
    fprintf(fid,'%d\t%d\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\t%f\n',...
            id,m,s_lat,s_lon,s_dep,s_O,c_latlon,c_latdep,c_londep,c_latO,c_lonO,c_depO);   
end

ee=load(outfile);m=size(ee,1);x=1:1:m;
disp('Number of events')
disp(m)
id=ee(:,1);y=ee(:,2);lat=ee(:,3);lon=ee(:,4);dep=ee(:,5);O=ee(:,6);
latlon=ee(:,7);latdep=ee(:,8);londep=ee(:,9);latO=ee(:,10);lonO=ee(:,11);depO=ee(:,12);
[id,index]=sort(id);y=y(index);lat=lat(index);lon=lon(index);dep=dep(index);O=O(index);
latlon=latlon(index);latdep=latdep(index);londep=londep(index);latO=latO(index);lonO=lonO(index);depO=depO(index);

fid2=fopen(logfile,'w');
fprintf(fid2,'%s %d %s %d\n','Number of events which location times more than ',mintimes,': ',m);
%average relative error
m_lat=mean(lat);m_lon=mean(lon);m_dep=mean(dep);m_O=mean(O);
fprintf(fid2,'%s %10.6f %10.6f %10.6f\n','Latitude/south-north average min max relative error of all events (km): ',m_lat,min(lat),max(lat));
fprintf(fid2,'%s %10.6f %10.6f %10.6f\n','Longitude/east-west average min max relative error of all events (km): ',m_lon,min(lon),max(lon));
fprintf(fid2,'%s %10.6f %10.6f %10.6f\n','Depth average min max relative error of all events (km): ',m_dep,min(dep),max(dep));
fprintf(fid2,'%s %10.6f %10.6f %10.6f\n','Origin time average min max relative error of all events (s): ',m_O,min(O),max(O));
%average correlation
%南北向-东西向
fprintf(fid2,'\n%s\n %10.6f  %10.6f  %10.6f  %10.6f\n','Average correlation of south-north and east-west: ',...
        mean(latlon),mean(latlon(latlon<0)),mean(latlon(latlon>=0)),mean(abs(latlon)));
fprintf(fid2,'%s\n %10.6f  %10.6f  %10.6f  %10.6f\n','STD of correlation of south-north and east-west: ',...
        std(latlon,1),std(latlon(latlon<0),1),std(latlon(latlon>=0),1),std(abs(latlon),1));
%南北向-深度
fprintf(fid2,'\n%s\n %10.6f  %10.6f  %10.6f  %10.6f\n','Average correlation of south-north and depth: ',...
        mean(latdep),mean(latdep(latdep<0)),mean(latdep(latdep>=0)),mean(abs(latdep)));
fprintf(fid2,'%s\n %10.6f  %10.6f  %10.6f  %10.6f\n','STD of correlation of south-north and depth: ',...
        std(latdep,1),std(latdep(latdep<0),1),std(latdep(latdep>=0),1),std(abs(latdep),1));   
%东西向-深度
fprintf(fid2,'\n%s\n %10.6f  %10.6f  %10.6f  %10.6f\n','Average correlation of east-west and depth: ',...
        mean(londep),mean(londep(londep<0)),mean(londep(londep>=0)),mean(abs(londep)));
fprintf(fid2,'%s\n %10.6f  %10.6f  %10.6f  %10.6f\n','STD of correlation of east-west and depth: ',...
        std(londep,1),std(londep(londep<0),1),std(londep(londep>=0),1),std(abs(londep),1));      
%南北向-发震时刻    
fprintf(fid2,'\n%s\n %10.6f  %10.6f  %10.6f  %10.6f\n','Average correlation of south-north and origin time: ',...
        mean(latO),mean(latO(latO<0)),mean(latO(latO>=0)),mean(abs(latO)));
fprintf(fid2,'%s\n %10.6f  %10.6f  %10.6f  %10.6f\n','STD of correlation of south-north and origin time: ',...
        std(latO,1),std(latO(latO<0),1),std(latO(latO>=0),1),std(abs(latO),1)); 
%东西向-发震时刻    
fprintf(fid2,'\n%s\n %10.6f  %10.6f  %10.6f  %10.6f\n','Average correlation of east-west and origin time: ',...
        mean(lonO),mean(lonO(lonO<0)),mean(lonO(lonO>=0)),mean(abs(lonO)));
fprintf(fid2,'%s\n %10.6f  %10.6f  %10.6f  %10.6f\n','STD of correlation of east-west and origin time: ',...
        std(lonO,1),std(lonO(lonO<0),1),std(lonO(lonO>=0),1),std(abs(lonO),1));    
%深度-发震时刻    
fprintf(fid2,'\n%s\n %10.6f  %10.6f  %10.6f  %10.6f\n','Average correlation of depth and origin time: ',...
        mean(depO),mean(depO(depO<0)),mean(depO(depO>=0)),mean(abs(depO)));
fprintf(fid2,'%s\n %10.6f  %10.6f  %10.6f  %10.6f\n','STD of correlation of depth and origin time: ',...
        std(depO,1),std(depO(depO<0),1),std(depO(depO>=0),1),std(abs(depO),1));
%%
%误差图频次图
dx=0.01;
bx=dx/2;
xe=(0:1:500);
xe=bx+xe.*dx;
n_lat=hist(lat,xe);n_lon=hist(lon,xe);n_dep=hist(dep,xe);
fid3=fopen('err3h.dat','w');
for i=1:size(xe,2)
    fprintf(fid3,'%f\t%d\t%d\t%d\n',xe(i),n_lat(i),n_lon(i),n_dep(i));   
end 
plot(xe,n_lat,'k-o',xe,n_lon,'k-s',xe,n_dep,'k-^');
xlabel('相对定位误差/km');ylabel('数量');legend('南北向','东西向','深度');xlim([0 5]);
%xlabel('Relative location error/km');ylabel('Counts');legend('南北向','东西向','深度');xlim([0 0.25]);
%title('Plot of sin(\Theta)')
%%
%相关性频次图
xc=(0:1:20);xc=-0.95+xc.*0.1;
% hist(latlon,xc);xlim([-1 1]);hold on
% hist(latdep,xc);xlim([-1 1]);
% hist(londep,xc);xlim([-1 1]);
% hist(latO,xc);xlim([-1 1]);
% hist(lonO,xc);xlim([-1 1]);
% hist(depO,xc);xlim([-1 1]);

% n_latlon=hist(latlon,xc);n_latdep=hist(latdep,xc);n_londep=hist(londep,xc);n_latO=hist(latO,xc);n_lonO=hist(lonO,xc);n_depO=hist(depO,xc);
% subplot(1,2,1)
% plot(xc,n_latlon,'k:o',xc,n_londep,'k-.s',xc,n_latdep,'k-^');xlim([-1 1]);
% xlabel('相关系数');ylabel('频次N');legend('南北向与东西向','东西向与深度','南北向与深度');
% subplot(1,2,2)
% plot(xc,n_latO,'k:o',xc,n_lonO,'k-.s',xc,n_depO,'k-^');xlim([-1 1]);
% xlabel('相关系数');ylabel('频次N');legend('南北向与发震时刻','东西向与发震时刻','深度与发震时刻');

a=latlon;b=latdep;c=londep;d=latO;e=lonO;f=depO;
n_latlon=hist(a,xc);n_latdep=hist(b,xc);n_londep=hist(c,xc);n_latO=hist(d,xc);n_lonO=hist(e,xc);n_depO=hist(f,xc);
subplot(1,2,1)
plot(xc,n_latlon,'k:o',xc,n_londep,'k-.s',xc,n_latdep,'k-^');xlim([-1 1]);
xlabel('相关系数');ylabel('频次N');legend('南北向与东西向','东西向与深度','南北向与深度');
subplot(1,2,2)
plot(xc,n_latO,'k:o',xc,n_lonO,'k-.s',xc,n_depO,'k-^');xlim([-1 1]);
xlabel('相关系数');ylabel('频次N');legend('南北向与发震时刻','东西向与发震时刻','深度与发震时刻');



% hist(depO,40);xlim([-1 1]);
% hold on
% plot(xc,n_depO);xlim([-1 1]);
% xlabel('相关系数');ylabel('数量');
% 
% p=polyfit(xc,n_depO,2);
% yc=p(1).*xc.*xc+p(2).*xc+p(3);
% plot(xc,yc);xlim([-1 1]);
% xlabel('相关系数');ylabel('数量');
%%
%误差：纬度-南北向
m_lat=mean(lat);
[AX,H1,H2]=plotyy(x,lat,x,y,'plot');
xlabel(['Num. of Events : ',num2str(m)]);title(['Latitude Std. mean = ',num2str(m_lat),'km']);
set(get(AX(1),'Ylabel'),'String','STD.(km)');set(get(AX(2),'Ylabel'),'String','Num.of data');
%set(AX(1),'XTickLabel',id);set(AX(2),'XTickLabel',id);
set(H2,'LineStyle','.','linewidth',2);set(H1,'LineStyle','.','linewidth',2);
%%
%误差：经度-东西向
m_lon=mean(lon);
[AX,H1,H2]=plotyy(x,lon,x,y,'plot');
xlabel(['Num. of Events : ',num2str(m)]);title(['Longitude Std. mean = ',num2str(m_lon),'km']);
set(get(AX(1),'Ylabel'),'String','STD.(km)');set(get(AX(2),'Ylabel'),'String','Num.of data');
%set(AX(1),'XTickLabel',id);set(AX(2),'XTickLabel',id);
set(H2,'LineStyle','.','linewidth',2);set(H1,'LineStyle','.','linewidth',2);
%%
%误差：深度-垂直向
m_dep=mean(dep);
[AX,H1,H2]=plotyy(x,dep,x,y,'plot');
xlabel(['Num. of Events : ',num2str(m)]);title(['Depth Std. mean = ',num2str(m_dep),'km']);
set(get(AX(1),'Ylabel'),'String','STD.(km)');set(get(AX(2),'Ylabel'),'String','Num.of data');
%set(AX(1),'XTickLabel',id);set(AX(2),'XTickLabel',id);
set(H2,'LineStyle','.','linewidth',2);set(H1,'LineStyle','.','linewidth',2);
%%
%误差：发震时刻
m_O=mean(O);
[AX,H1,H2]=plotyy(x,O,x,y,'plot');
xlabel(['Num. of Events : ',num2str(m)]);title(['Otime Std. mean = ',num2str(m_O),'s']);
set(get(AX(1),'Ylabel'),'String','STD.(s)');set(get(AX(2),'Ylabel'),'String','Num.of data');
%set(AX(1),'XTickLabel',id);set(AX(2),'XTickLabel',id);
set(H2,'LineStyle','.','linewidth',2);set(H1,'LineStyle','.','linewidth',2);
%%
%相关性：水平向
mn=mean(latlon);std0=std(latlon,1);
[AX,H1,H2]=plotyy(x,latlon,x,y,'plot');
xlabel(['Num. of Events : ',num2str(m)]);title(['Lat-Lon CC mean = ',num2str(mn),'   std = ',num2str(std0)]);
set(get(AX(1),'Ylabel'),'String','CC');set(get(AX(2),'Ylabel'),'String','Num.of data');
%set(AX(1),'XTickLabel',id);set(AX(2),'XTickLabel',id);
set(H2,'LineStyle','.','linewidth',2);set(H1,'LineStyle','.','linewidth',2);
%%
%相关性：南北向-深度
mn=mean(latdep);std0=std(latdep,1);
[AX,H1,H2]=plotyy(x,latdep,x,y,'plot');
xlabel(['Num. of Events : ',num2str(m)]);title(['Lat-Dep CC mean = ',num2str(mn),'   std = ',num2str(std0)]);
set(get(AX(1),'Ylabel'),'String','CC');set(get(AX(2),'Ylabel'),'String','Num.of data');
%set(AX(1),'XTickLabel',id);set(AX(2),'XTickLabel',id);
set(H2,'LineStyle','.','linewidth',2);set(H1,'LineStyle','.','linewidth',2);
%%
%相关性：东西向-深度
mn=mean(londep);std0=std(londep,1);
[AX,H1,H2]=plotyy(x,londep,x,y,'plot');
xlabel(['Num. of Events : ',num2str(m)]);title(['Lon-Dep CC mean = ',num2str(mn),'   std = ',num2str(std0)]);
set(get(AX(1),'Ylabel'),'String','CC');set(get(AX(2),'Ylabel'),'String','Num.of data');
%set(AX(1),'XTickLabel',id);set(AX(2),'XTickLabel',id);
set(H2,'LineStyle','.','linewidth',2);set(H1,'LineStyle','.','linewidth',2);
%%
%相关性：南北向-发震时刻
mn=mean(latO);std0=std(latO,1);
[AX,H1,H2]=plotyy(x,latO,x,y,'plot');
xlabel(['Num. of Events : ',num2str(m)]);title(['Lat-O CC mean = ',num2str(mn),'   std = ',num2str(std0)]);
set(get(AX(1),'Ylabel'),'String','CC');set(get(AX(2),'Ylabel'),'String','Num.of data');
%set(AX(1),'XTickLabel',id);set(AX(2),'XTickLabel',id);
set(H2,'LineStyle','.','linewidth',2);set(H1,'LineStyle','.','linewidth',2);
%%
%相关性：东西向-发震时刻
mn=mean(lonO);std0=std(lonO,1);
[AX,H1,H2]=plotyy(x,lonO,x,y,'plot');
xlabel(['Num. of Events : ',num2str(m)]);title(['Lon-O CC mean = ',num2str(mn),'   std = ',num2str(std0)]);
set(get(AX(1),'Ylabel'),'String','CC');set(get(AX(2),'Ylabel'),'String','Num.of data');
%set(AX(1),'XTickLabel',id);set(AX(2),'XTickLabel',id);
set(H2,'LineStyle','.','linewidth',2);set(H1,'LineStyle','.','linewidth',2);
%%
%相关性：深度-发震时刻
mn=mean(depO);std0=std(depO,1);
[AX,H1,H2]=plotyy(x,depO,x,y,'plot');
xlabel(['Num. of Events : ',num2str(m)]);title(['Dep-O CC mean = ',num2str(mn),'   std = ',num2str(std0)]);
set(get(AX(1),'Ylabel'),'String','CC');set(get(AX(2),'Ylabel'),'String','Num.of data');
%set(AX(1),'XTickLabel',id);set(AX(2),'XTickLabel',id);
set(H2,'LineStyle','.','linewidth',2);set(H1,'LineStyle','.','linewidth',2);








