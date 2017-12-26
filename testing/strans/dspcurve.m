clear
dist = '194.33km';
load ('A111A602.dsp');
load ('ST.A111A602.dsp');

plot(A111A602(:,1),A111A602(:,2),'ro',ST_A111A602(:,1),ST_A111A602(:,2),'b*','LineWidth',1.0);
%plot(ST_A111A602(:,1),ST_A111A602(:,2),'b','LineWidth',1.5);
legend('A111A602','ST.A111A602');axis([4 40 2 5]);xlabel('Period(s)');ylabel('U(km/s)');title(['DIST = ',dist])