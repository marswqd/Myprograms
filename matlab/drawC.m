clear
%% 1-2
clear
dist = 238.99;
load A101.A608.-.SAC-T_C.txt;
load ST.A101.A608.-.SAC-T_C.txt; 
load DIF.A101.A608.A.SAC-T_C.txt; 
load ST.A101.A608.3-.SAC-T_C.txt;
semilogx(A101_A608___SAC_T_C(1:37,1),A101_A608___SAC_T_C(1:37,2),'b.',...
         ST_A101_A608___SAC_T_C(1:37,1),ST_A101_A608___SAC_T_C(1:37,2),'ro');
hold on;
line([dist/10,dist/10],[2,5],'Color','k','LineWidth',1.5);
legend('Two-Station','ST-Two-Station');
axis([7 dist/10 2 5]);xlabel('Period(s)','fontsize',16);ylabel('C(km/s)','fontsize',16);title(['A101.A608.SAC   DIST = ',num2str(dist),'km'],'fontsize',16)
hold off;
pause
semilogx(ST_A101_A608___SAC_T_C(1:37,1),ST_A101_A608___SAC_T_C(1:37,2),'ro',...
         DIF_A101_A608_A_SAC_T_C(1:37,1),DIF_A101_A608_A_SAC_T_C(1:37,2),'b*',...
         ST_A101_A608_3__SAC_T_C(1:37,1),ST_A101_A608_3__SAC_T_C(1:37,2),'m^');
hold on;
line([dist/10,dist/10],[2,5],'Color','k','LineWidth',1.5);
legend('ST-Two-Station','DIF-Two-Station','ST-Three-Station');
axis([7 dist/10 2 5]);xlabel('Period(s)','fontsize',16);ylabel('C(km/s)','fontsize',16);title(['A101.A608.SAC   DIST = ',num2str(dist),'km'],'fontsize',16)
hold off;
pause
semilogx(ST_A101_A608___SAC_T_C(1:37,1),ST_A101_A608___SAC_T_C(1:37,2),'ro',...
         DIF_A101_A608_A_SAC_T_C(1:37,1),DIF_A101_A608_A_SAC_T_C(1:37,2),'b*');
hold on;
line([dist/10,dist/10],[2,5],'Color','k','LineWidth',1.5);
legend('ST-Two-Station','DIF-Two-Station');
axis([7 dist/10 2 5]);xlabel('Period(s)','fontsize',16);ylabel('C(km/s)','fontsize',16);title(['A101.A608.SAC   DIST = ',num2str(dist),'km'],'fontsize',16)
hold off;
STDIF(:,1) = ST_A101_A608___SAC_T_C(:,1);
STDIF(:,2) = ST_A101_A608___SAC_T_C(:,2)-DIF_A101_A608_A_SAC_T_C(:,2);
ST3 = ST_A101_A608___SAC_T_C(:,2)-ST_A101_A608_3__SAC_T_C(:,2);
DIF3 = ST_A101_A608_3__SAC_T_C(:,2)-DIF_A101_A608_A_SAC_T_C(:,2);
%% 2-3
clear
dist = 236.02;
load A608.L236.-.SAC-T_C.txt; 
load ST.A608.L236.-.SAC-T_C.txt; 
load DIF.A608.L236.A.SAC-T_C.txt; 
load ST.A608.L236.3-.SAC-T_C.txt;
semilogx(A608_L236___SAC_T_C(1:37,1),A608_L236___SAC_T_C(1:37,2),'b.',...
         ST_A608_L236___SAC_T_C(1:37,1),ST_A608_L236___SAC_T_C(1:37,2),'ro');
hold on;
line([dist/10,dist/10],[2,5],'Color','k','LineWidth',1.5);
legend('Two-Station','ST-Two-Station');
axis([7 dist/10 2 5]);xlabel('Period(s)','fontsize',16);ylabel('C(km/s)','fontsize',16);title(['A608.L236.SAC   DIST = ',num2str(dist),'km'],'fontsize',16)
hold off;
pause
semilogx(ST_A608_L236___SAC_T_C(1:37,1),ST_A608_L236___SAC_T_C(1:37,2),'ro',...
         DIF_A608_L236_A_SAC_T_C(1:37,1),DIF_A608_L236_A_SAC_T_C(1:37,2),'b*',...
         ST_A608_L236_3__SAC_T_C(1:37,1),ST_A608_L236_3__SAC_T_C(1:37,2),'m^');
hold on;
line([dist/10,dist/10],[2,5],'Color','k','LineWidth',1.5);
legend('ST-Two-Station','DIF-Two-Station','ST-Three-Station');
axis([7 dist/10 2 5]);xlabel('Period(s)','fontsize',16);ylabel('C(km/s)','fontsize',16);title(['A608.L236.SAC   DIST = ',num2str(dist),'km'],'fontsize',16)
hold off;
pause
semilogx(ST_A608_L236___SAC_T_C(1:37,1),ST_A608_L236___SAC_T_C(1:37,2),'ro',...
         DIF_A608_L236_A_SAC_T_C(1:37,1),DIF_A608_L236_A_SAC_T_C(1:37,2),'b*');
hold on;
line([dist/10,dist/10],[2,5],'Color','k','LineWidth',1.5);
legend('ST-Two-Station','DIF-Two-Station');
axis([7 dist/10 2 5]);xlabel('Period(s)','fontsize',16);ylabel('C(km/s)','fontsize',16);title(['A608.L236.SAC   DIST = ',num2str(dist),'km'],'fontsize',16)
hold off;
STDIF(:,1) = ST_A608_L236___SAC_T_C(:,1);
STDIF(:,2) = ST_A608_L236___SAC_T_C(:,2)-DIF_A608_L236_A_SAC_T_C(:,2);
%% 1-3
clear
dist = 474.98;
load A101.L236.-.SAC-T_C.txt; 
load ST.A101.L236.-.SAC-T_C.txt; 
load DIF.A101.L236.A.SAC-T_C.txt; 
%load ST.K030.K051.3-.SAC-T_C.txt;
semilogx(A101_L236___SAC_T_C(1:37,1),A101_L236___SAC_T_C(1:37,2),'b.',...
         ST_A101_L236___SAC_T_C(1:37,1),ST_A101_L236___SAC_T_C(1:37,2),'ro');
hold on;
line([dist/10,dist/10],[2,5],'Color','k','LineWidth',1.5);
legend('Two-Station','ST-Two-Station');
axis([7 dist/10 2 5]);xlabel('Period(s)','fontsize',16);ylabel('C(km/s)','fontsize',16);title(['A101.L236.SAC   DIST = ',num2str(dist),'km'],'fontsize',16)
hold off;
pause
semilogx(ST_A101_L236___SAC_T_C(1:37,1),ST_A101_L236___SAC_T_C(1:37,2),'ro',...
         DIF_A101_L236_A_SAC_T_C(1:37,1),DIF_A101_L236_A_SAC_T_C(1:37,2),'b*');
hold on;
line([dist/10,dist/10],[2,5],'Color','k','LineWidth',1.5);
legend('ST-Two-Station','DIF-Two-Station');
axis([7 dist/10 2 5]);xlabel('Period(s)','fontsize',16);ylabel('C(km/s)','fontsize',16);title(['A101.L236.SAC   DIST = ',num2str(dist),'km'],'fontsize',16)
hold off;
STDIF(:,1) = ST_A101_L236___SAC_T_C(:,1);
STDIF(:,2) = ST_A101_L236___SAC_T_C(:,2)-DIF_A101_L236_A_SAC_T_C(:,2);



