clear
load F:\��������\������������ͼ\Ƶɢ����\A111A408.txt
load F:\��������\������������ͼ\Ƶɢ����\A001A209.txt
load F:\��������\������������ͼ\Ƶɢ����\A202L207.txt
load F:\��������\������������ͼ\Ƶɢ����\A704L228.txt
load F:\��������\������������ͼ\Ƶɢ����\K023L207.txt
load F:\��������\������������ͼ\Ƶɢ����\L209L228.txt
semilogx(A111A408(:,1),A111A408(:,2),'r','LineWidth',1.5);
%legend('A111A408')
hold on

semilogx(A001A209(:,1),A001A209(:,2),'g','LineWidth',1.5);
%legend('A001A209')
semilogx(A202L207(:,1),A202L207(:,2),'-.b','LineWidth',1.5);
%legend('A202L207')

semilogx(A704L228(:,1),A704L228(:,2),'k','LineWidth',1.5);
%legend('A704L228')

semilogx(K023L207(:,1),K023L207(:,2),'m','LineWidth',1.5);
%legend('K023L207')

semilogx(L209L228(:,1),L209L228(:,2),':k','LineWidth',1.5);
%legend('L209L228')
legend('A111A408','A001A209','A202L207','A704L228','K023L207','L209L228')

axis([4 50 2 5]);
xlabel('Period(s)')
ylabel('U(km/s)')