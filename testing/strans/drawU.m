clear
%% 1-2
clear
dist = 238.99;
load A101A608.-.dsp;
load ST.A101A608.-.dsp; 
load DIF.A101A608.A.dsp; 
% load ST.A101A608.3-.dsp;
% semilogx(ST_A101A608__(:,1),ST_A101A608__(:,2),'ro',DIF_A101A608_A(:,1),DIF_A101A608_A(:,2),'b*',ST_A101A608_3_(:,1),ST_A101A608_3_(:,2),'m^');
% hold on;
% line([dist/10,dist/10],[2,5],'Color','k','LineWidth',1.5);
% legend('ST-Two-Station','DIF-Two-Station','ST-Three-Station');
% axis([4 dist/10 2 5]);xlabel('Period(s)','fontsize',16);ylabel('U(km/s)','fontsize',16);title(['A101.A608.SAC   DIST = ',num2str(dist),'km'],'fontsize',16);
% hold off;
% pause
semilogx(ST_A101A608__(:,1),ST_A101A608__(:,2),'ro',DIF_A101A608_A(:,1),DIF_A101A608_A(:,2),'b*');
hold on;
line([dist/10,dist/10],[2,5],'Color','k','LineWidth',1.5);
legend('ST-Two-Station','DIF-Two-Station');
axis([4 dist/10 2 5]);xlabel('Period(s)','fontsize',16);ylabel('U(km/s)','fontsize',16);title(['A101.A608.SAC   DIST = ',num2str(dist),'km'],'fontsize',16);
hold off;
pause
semilogx(A101A608__(:,1),A101A608__(:,2),'b.',ST_A101A608__(:,1),ST_A101A608__(:,2),'ro');
hold on;
line([dist/10,dist/10],[2,5],'Color','k','LineWidth',1.5);
legend('Two-Station','ST-Two-Station');
axis([4 dist/10 2 5]);xlabel('Period(s)','fontsize',16);ylabel('U(km/s)','fontsize',16);title(['A101.A608.SAC   DIST = ',num2str(dist),'km'],'fontsize',16);
hold off;
%% 2-3
clear
dist = 236.02;
load A608L236.-.dsp; 
load ST.A608L236.-.dsp; 
load DIF.A608L236.A.dsp; 
% load ST.A608L236.3-.dsp;
% semilogx(ST_A608L236__(:,1),ST_A608L236__(:,2),'ro',DIF_A608L236_A(:,1),DIF_A608L236_A(:,2),'b*',ST_A608L236_3_(:,1),ST_A608L236_3_(:,2),'m^');
% hold on;
% line([dist/10,dist/10],[2,5],'Color','k','LineWidth',1.5);
% legend('ST-Two-Station','DIF-Two-Station','ST-Three-Station');
% axis([4 dist/10 2 5]);xlabel('Period(s)','fontsize',16);ylabel('U(km/s)','fontsize',16);title(['A608.L236.SAC   DIST = ',num2str(dist),'km'],'fontsize',16);
% hold off;
% pause
semilogx(ST_A608L236__(:,1),ST_A608L236__(:,2),'ro',DIF_A608L236_A(:,1),DIF_A608L236_A(:,2),'b*');
hold on;
line([dist/10,dist/10],[2,5],'Color','k','LineWidth',1.5);
legend('ST-Two-Station','DIF-Two-Station');
axis([4 dist/10 2 5]);xlabel('Period(s)','fontsize',16);ylabel('U(km/s)','fontsize',16);title(['A608.L236.SAC   DIST = ',num2str(dist),'km'],'fontsize',16);
hold off;
pause
semilogx(A608L236__(:,1),A608L236__(:,2),'b.',ST_A608L236__(:,1),ST_A608L236__(:,2),'ro');
hold on;
line([dist/10,dist/10],[2,5],'Color','k','LineWidth',1.5);
legend('Two-Station','ST-Two-Station');
axis([4 dist/10 2 5]);xlabel('Period(s)','fontsize',16);ylabel('U(km/s)','fontsize',16);title(['A608.L236.SAC   DIST = ',num2str(dist),'km'],'fontsize',16);
hold off;
%% 1-3
clear
dist = 474.98;
load A101L236.-.dsp; 
load ST.A101L236.-.dsp; 
load DIF.A101L236.A.dsp; 
semilogx(ST_A101L236__(:,1),ST_A101L236__(:,2),'ro',DIF_A101L236_A(:,1),DIF_A101L236_A(:,2),'b*');
hold on;
line([dist/10,dist/10],[2,5],'Color','k','LineWidth',1.5);
legend('ST-Two-Station','DIF-Two-Station');
axis([4 dist/10 2 5]);xlabel('Period(s)','fontsize',16);ylabel('U(km/s)','fontsize',16);title(['A101.L236.SAC   DIST = ',num2str(dist),'km'],'fontsize',16);
hold off;
pause
semilogx(A101L236__(:,1),A101L236__(:,2),'b.',ST_A101L236__(:,1),ST_A101L236__(:,2),'ro');
hold on;
line([dist/10,dist/10],[2,5],'Color','k','LineWidth',1.5);
legend('Two-Station','ST-Two-Station');
axis([4 dist/10 2 5]);xlabel('Period(s)','fontsize',16);ylabel('U(km/s)','fontsize',16);title(['A101L.236.SAC   DIST = ',num2str(dist),'km'],'fontsize',16);
hold off;


