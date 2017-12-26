c = load('SREGN.out'); mfc = load('c24.sac.MFC.out');
c2 = load('Z24.0.sac.C.txt');
plot(c(:,3),c(:,5),'r',mfc(:,1),mfc(:,3),'b',c2(:,1),c2(:,3),'k');xlim([2 200])


