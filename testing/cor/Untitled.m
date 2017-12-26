cor=load('cor0.5.out');
cor=load('cor0.1.out');
cor=load('cor0.05.out');
cor=load('cor0.005.out');
cor=load('cor0.0.out');
N = length(cor(:,1)); x = (1:1:N);
plot(x,cor(:,1),'k',x,cor(:,2),'b',x,cor(:,3),'r');legend('T','F','S');
plot(x,abs(cor(:,4)),'b',x,abs(cor(:,5)),'r');legend('F','S');ylim([0 0.1]);


plot(x,abs(cor0_5(:,4)),'b',x,abs(cor0_5(:,5)),'r');legend('F','S');ylim([0 0.1]);
plot(x,abs(cor0_1(:,4)),'b',x,abs(cor0_1(:,5)),'r');legend('F','S');ylim([0 0.1]);
plot(x,abs(cor0_05(:,4)),'b',x,abs(cor0_05(:,5)),'r');legend('F','S');ylim([0 0.1]);
plot(x,abs(cor0_005(:,4)),'b',x,abs(cor0_005(:,5)),'r');legend('F','S');ylim([0 0.1]);

