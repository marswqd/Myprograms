!����: �����������������൹ʱ��,���ɳ�ʼ�������ʱ���ļ�:mcd.txt,����ccme.f90
!  ע: ���������ݱ�����ͬһ̨վ��¼����(���ݲ������һ��)
!      �����SAC�ļ�������ʽΪ:P.BAS.20120907121632.Z.SAC
!      �˲�+���������+��������˫������+ʱ����ػ���ؼ����(detector)����ʱ��
!      (������ʱ�����:-0.5-1s,˫������������:1.5s and 2s)
!      correlation detector: Schaff-2005-Waveform Cross-Correlation-Based Differential
!                  Travel-Time Measurements at the Northern California Seismic Network
!      mcd.in������Z�����ļ���; epairΪҪ����ĵ����¼��Ե�id,ÿ��һ���¼���
!      ZR��������P������ʱ��,RT��������S������ʱ��; ʱ��Ϊ����ʾ����1����ʱ��::tt1-tt2
!����: ���嶫 ʱ��: 2015/8/24 21:04:07
program mcd
 implicit none
 integer*4::i,j,ii,jj,i1,j1,i2,j2,k,kk,k1,k2,m,n,mm,nn,num,nerr,neve,nep
 integer*4::NPTS1,NPTS2,ncor,ID1,ID2,iphase,f0,slidewin
 integer*4::wln1,wrn1,wn,wln2,wrn2,sln1,srn1,sln2,srn2,wln,wrn,cn1,cn2 !wn:ʱ�䴰�ڵ����ݵ���,�����2����
 integer*4::sln11,srn11,sln22,srn22,cn11,cn22
 !integer*4::wln11,wrn11,wnn,wln22,wrn22,sln11,srn11,sln22,srn22
 integer*4,allocatable::pos(:,:),sta(:),id(:),aid(:),epid(:,:)
 real*4::B1,O1,P1,S1,B2,O2,P2,S2,DT,evla1,evlo1,evla2,evlo2,dep1,dep2,dist1,dist2   !����ͷ�ļ�����
 real*4::T1,T2,odt,sdt1,sdt11,sdt2,sdt22,cdt1,cdt2,tdt1,tdt2,pdt1,pdt2,cmax1,cmax2
 real*4::cc,ccut,d12,az1,az2,az,alat,alon,dlat,dlon,stla,stlo,slidel1,slider1,slidel2,slider2
 real*4::f1,f2,f3,f4,pwl,pwr,swl,swr,sld1,sld2,slidel,slider,maxdist,maxsep,vp,vs,dtt,aa,bb,cr,cz,ct
 real*4,parameter::PI=3.1415926535898e0
 real*4,allocatable::epdist(:)
 character::flag*1,KP1*8,KS1*8,tempc*14,name1*60,name2*60,epair*60,flist*60,card*100,epname*60
 character*60::nmr1,nmt1,nmz1,nmr2,nmt2,nmz2,na,naid1,naid2
 character,allocatable::sacname(:)*60!,eve(:)*14
 logical::ex

 real*4,allocatable::dr1(:),dt1(:),dz1(:),dr2(:),dt2(:),dz2(:) !��������
 real*4,allocatable::cr1(:),ct1(:),cz1(:),cr2(:),ct2(:),cz2(:)
 !real*4,allocatable::cr11(:),ct11(:),cz11(:),cr22(:),ct22(:),cz22(:) !Ҫ���л���ؼ��������
 !real*4,allocatable::cr12(:),ct12(:),cz12(:),c12(:)
 !real*4,allocatable::cr1122(:),ct1122(:),cz1122(:),c1122(:)     !���������
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)


 call CPU_TIME(T1)
 !open(10,file='mcd.log')


!��ȡ�����ļ���Ϣ
 open(11,file='mcd.in',status='old',action='read')
 read(11,'(a)') epair       !�����¼����ļ�:   # id1 id2 dist12
 read(11,'(a100)') card
 read(card,*) f0
 if(f0==1) read(card,*) f0,f1,f2,f3,f4 !f0=1:��ͨ�˲�(Hz),1-2-6-8
 ! iphase=1 P only ; iphase=2 S only ; iphase=else P and S
 read(11,*) iphase,pwl,pwr,swl,swr !P S ��ʼʱ��λ���趨,���ൽʱ����ƫ����(s)
 ! һ�� pwl=-0.5e0 pwr=1.0e0 swl=-0.5e0 swr=1.0e0
 ! slidewin=2 ˫������ ; slidewin=else ��������
 read(11,*) slidewin,sld1,sld2   !������,һ��sld1=1.5e0 sld2=2.0e0
 read(11,*) maxdist,maxsep,ccut  !̨վ����Դ������250km;�����Դ�������10km;���ϵ����ֵֹ0.5;
 !read(11,*) num
 !allocate(sacname(num),eve(num))
 read(11,'(a)') flist     !�洢Ҫ����Ĳ����ļ���
 close(11)


 open(11,file=trim(flist),status='old',action='read')
 read(11,*) num
 print*, 'SAC Z-com files: ',num
 allocate(sacname(num))
 do i=1,num
  read(11,*) sacname(i)
  j=index(sacname(i),'.Z.')
  if(j==0) then
   print*, 'The input is not Z component, please check!'
   stop
  endif
  nmz1=sacname(i)(1:j)//'Z.SAC'
  nmr1=sacname(i)(1:j)//'R.SAC'
  nmt1=sacname(i)(1:j)//'T.SAC'
  call exist(nmz1)
  call exist(nmr1)
  call exist(nmt1)
  call exist(sacname(i))
  sacname(i)=adjustl(sacname(i))
  !print*, trim(sacname(i))
 enddo
 close(11)


 !������������
 !pwl=-0.5e0
 !pwr=0.5e0
 !swl=-1.0e0
 !swr=1.0e0
 !sld1=1.0e0
 !sld2=1.5e0

 !write(10,*) 'Parameter setting:'
 !if(f0==1) then
  !write(10,*) 'Freq: ',f1,f2,f3,f4
 !endif
 !write(10,*) 'iphase: ',iphase,pwl,pwr,swl,swr
 !write(10,*) 'slide: ',sld1,sld2
 !write(10,*) 'maxdist: ',maxdist,'maxsep: ',maxsep,'ccut: ',ccut
 !write(10,*) '-----'

 !open(12,file='mcd.txt')   !��ʼ�������ʱ���ļ�


 print*, 'Step 0: Read event-pair information'
 open(11,file=trim(epair),status='old',action='read')
 call filelen(11,nep)
 print*, 'event-pairs: ',nep
 allocate(epid(nep,2),epdist(nep))
 do i=1,nep
  read(11,*) flag,epid(i,1),epid(i,2),epdist(i)
 enddo
 close(11)


 print*, 'Step 1: Determine the position'
!�ҵ�SAC�ļ����������¼���������sacname�����е�λ��
 inquire(file='idpos.txt',exist=ex)
 if(ex) then
  print*, 'idpos file exit, read it'
  open(11,file='idpos.txt',status='old',action='read')
  call filelen(11,m)
  do i1=1,m
   read(11,*) i
  enddo
  neve=i
  allocate(id(num),pos(200,neve),sta(neve))
  rewind(11)
  do i1=1,m
   read(11,*) i,j,k,k1
   id(i)=j
   sta(i)=k
   pos(k,i)=k1
  enddo
  close(11)
  goto 30
 endif
 allocate(id(num),aid(num))
 neve=1
 name1=sacname(1)
 !call dot(name1,2,m)
 !call dot(name1,3,n)
 !eve(neve)=name1(m+1:n-1)
 call brsach(100,name1,nerr)
 id(neve)=ihdr(13)
 aid(1)=ihdr(13)
 i=2
 20 do while(i<=num)
  name1=sacname(i)
  i=i+1
  !call dot(name1,2,m)
  !call dot(name1,3,n)
  !tempc=name1(m+1:n-1)
  call brsach(100,name1,nerr)
  aid(i-1)=ihdr(13)
  do j=1,neve
   !if(tempc==eve(j)) goto 20
   if(aid(i-1)==id(j)) goto 20
  enddo
  neve=neve+1
  id(neve)=aid(i-1)
  !eve(neve)=name1(m+1:n-1)
 enddo
 print*, 'events in SAC files: ',neve
 !write(10,*) 'There are ',neve,' events.'
 !write(10,*) '-----'
!�ҵ�ÿ���¼���Ӧ��SAC�ļ���sacname����Ķ�Ӧ��ϵ
 allocate(pos(200,neve),sta(neve))
 do i=1,neve
  k=0
  do j=1,num
   !name1=sacname(j)
   !call dot(name1,2,m)
   !call dot(name1,3,n)
   !if(name1(m+1:n-1)==eve(i)) then
    !k=k+1
    !pos(k,i)=j  !��i���¼��ĵ�k��̨վλ��sacname�ĵ�j��
   !endif
   if(aid(j)==id(i)) then
    k=k+1
    pos(k,i)=j  !��i���¼��ĵ�k��̨վλ��sacname�ĵ�j��
   endif
  enddo
  sta(i)=k      !��i���¼���k��̨վ.������¼��
 enddo
 deallocate(aid)
 open(11,file='idpos.txt')
 print*, 'creating idpos file exit'
 do i=1,neve
  do k=1,sta(i)
   write(11,*) i,id(i),k,pos(k,i)
  enddo
 enddo
 close(11)
 30 continue

! DT=rhdr(1)     B=rhdr(6)      O=rhdr(8)      DIST=rhdr(51)
! A=rhdr(9)      T0=rhdr(11)    T1=rhdr(12)    T2=rhdr(13)
! STLA=rhdr(32)  STLO=rhdr(33)  STEL=rhdr(34)  STDP=rhdr(35)
! EVLA=rhdr(36)  EVLO=rhdr(37)  EVEL=rhdr(38)  EVDP=rhdr(39)
! NZYEAR=ihdr(1) NZJDAY=ihdr(2) NZHOUR=ihdr(3) NZMIN=ihdr(4)
! NZSEC=ihdr(5)  NZMSEC=ihdr(6) NPTS=ihdr(10)  KSTNM=chdr(1)

 print*, 'Step 2: Calculate time difference'
 !���㲨�λ������ʱ��
 do i=1,nep
  write(na,'(i20)') i
  na=adjustl(na)
  write(naid1,'(i10)') epid(i,1)
  naid1=adjustl(naid1)
  write(naid2,'(i10)') epid(i,2)
  naid2=adjustl(naid2)
  epname='ep_'//trim(na)//'.'//trim(naid1)//'.'//trim(naid2)//'.txt'
  !call namegen('ep_','',i,'txt',epname)
  inquire(file='../'//trim(epname),exist=ex)
  if(ex) cycle
  ii=-1
  do j=1,neve
   if(epid(i,1)==id(j)) then
    ii=j
    exit
   endif
  enddo
  if(ii==-1) cycle
  jj=-1
  do j=1,neve
   if(epid(i,2)==id(j)) then
    jj=j
    exit
   endif
  enddo
  if(jj==-1) cycle
  ID1=epid(i,1)
  ID2=epid(i,2)
  d12=epdist(i)
  !i1=pos(1,ii)
  !call brsach(100,sacname(i1),nerr)
  !evla1=rhdr(36) !�¼�γ��
  !evlo1=rhdr(37) !�¼�����
  !dep1=rhdr(39)  !��Դ���
  !ID1=ihdr(13)
  !i2=pos(1,jj)
  !call brsach(100,sacname(i2),nerr)
  !evla2=rhdr(36)
  !evlo2=rhdr(37)
  !dep2=rhdr(39)
  !ID2=ihdr(13)
  !dlat=evla1-evla2
  !dlon=evlo1-evlo2
  !d12=sqrt((dlat*111.195)**2+(dlon*(cos(evla1*PI/180)*111.195))**2+(dep1-dep2)**2) !�㷨����ph2dt.f90
  !alat=(evla1+evla2)/2
  !alon=(evlo1+evlo2)/2
  !if(d12>=dist1/5.or.d12>=dist2/5) then    !Ҫ���¼����ԶС�����о�
  if(d12>maxsep) then                       !Ҫ���¼����ԶС��maxsep
   print*, 'Event spacing does not meet the requirements'
   !write(10,*) 'The event spacing between ',trim(name1),' and ',trim(name2),' does not meet the requirements'
   cycle
  endif
  !epname=namegen('ep','',i,'txt')
  print*, trim(epname)
  open(13,file='../'//trim(epname))
  if(ID1<ID2) then
   print*, '#',ID1,ID2,d12
   !write(10,*) '#',ID1,ID2,d12
   !write(12,*) '#',ID1,ID2,d12
   !write(13,*) '#',ID1,ID2,d12
  else
   print*, '#',ID2,ID1,d12
   !write(10,*) '#',ID2,ID1,d12
   !write(12,*) '#',ID2,ID1,d12
   !write(13,*) '#',ID2,ID1,d12
  endif
  do i1=1,sta(ii)
   j1=pos(i1,ii)
   name1=sacname(j1)
   !��ȡ����ؼ����е����ļ�
   call brsach(100,name1,nerr)
   NPTS1=ihdr(10)
   DT=rhdr(1)     !�������
   B1=rhdr(6)     !��һ���������ʱ��
   O1=rhdr(8)     !����ʱ��
   P1=rhdr(9)     !A���ൽʱ,һ��ΪP����ʱ
   S1=rhdr(11)    !T0���ൽʱ,һ��ΪS����ʱ
   dist1=rhdr(51) !���о�
   az1=rhdr(52)   !�¼���̨վ�ķ�λ��(��)AZ
   KP1=chdr(6)    !A����
   KS1=chdr(7)    !T0����
   stla=rhdr(32)  !̨վγ��
   stlo=rhdr(33)  !̨վ����
   if(O1==-12345.0e0) then  !�޷���ʱ��
    print*, 'The O time is unknow: ',trim(name1)
    !write(10,*) 'The O time of ',trim(name1),' is unknow, please check!'
    cycle
   endif
   if(P1==-12345.0e0.and.S1==-12345.0e0) then  !��������Ϣ
    print*, 'The phase info is unknow: ',trim(name1)
    !write(10,*) 'The phase info of ',trim(name1),' is unknow, please check!'
    cycle
   endif
   if(dist1>maxdist) then  !Ҫ�����о�С��maxdist:500km
    print*, 'Epicentral distance is too large: ',trim(name1)
    !write(10,*), 'The epicentral distance of ',trim(name1),' is too large'
    cycle
   endif
   !���¼�eve(j)��Ѱ���¼�eve(i)����̨ͬվ.�����ļ�¼
   call dot(name1,1,mm)
   call dot(name1,2,nn)
   call dot(name1,3,kk)
   do i2=1,sta(jj)
    j2=pos(i2,jj)
    name2=sacname(j2)    !��ͬ�¼�����̨ͬվ.������¼
    if(name1(mm+1:nn-1)==name2(mm+1:nn-1).and.name1(kk+1:kk+1)==name2(kk+1:kk+1)) then
     !��ȡ����ؼ����еĸ��ļ�
     call brsach(100,name2,nerr)
     NPTS2=ihdr(10)
     ID2=ihdr(13)
     DT=rhdr(1)
     B2=rhdr(6)
     O2=rhdr(8)
     P2=rhdr(9)
     S2=rhdr(11)
     dist2=rhdr(51)
     az2=rhdr(52)
     if(O2==-12345.0e0) then  !�޷���ʱ��
      print*, 'The O time is unknow: ',trim(name2)
      !write(10,*) 'The O time of ',trim(name2),' is unknow, please check!'
      exit
     endif
     if(P2==-12345.0e0.and.S2==-12345.0e0) then  !��������Ϣ
      print*, 'The phase info is unknow: ',trim(name2)
      !write(10,*) 'The phase info of ',trim(name2),' is unknow, please check!'
      exit
     endif
     if(dist2>maxdist) then  !Ҫ�����о�С��maxdist:500km
      print*, 'Epicentral distance is too large: ',trim(name2)
      !write(10,*), 'The epicentral distance of ',trim(name2),' is too large'
      exit
     endif
     if(ID1<ID2) then
      print*, name1(mm+1:nn-1)//'.'//name1(kk+1:kk+1),dist1,dist2
      !write(10,*) trim(name1),'  ',trim(name2)
      !write(10,*) 'DIST:',dist1,dist2,'AZ:',az1,az2
     else
      print*, name1(mm+1:nn-1)//'.'//name1(kk+1:kk+1),dist2,dist1
      !write(10,*) trim(name2),'  ',trim(name1)
      !write(10,*) 'DIST:',dist2,dist1,'AZ:',az2,az1
     endif
     !��ȡ��������
     k1=index(name1,'.Z.')
     nmz1=name1(1:k1)//'Z.SAC'
     nmr1=name1(1:k1)//'R.SAC'
     nmt1=name1(1:k1)//'T.SAC'
     k2=index(name2,'.Z.')
     nmz2=name2(1:k2)//'Z.SAC'
     nmr2=name2(1:k2)//'R.SAC'
     nmt2=name2(1:k2)//'T.SAC'
     allocate(dr1(NPTS1),dt1(NPTS1),dz1(NPTS1),dr2(NPTS2),dt2(NPTS2),dz2(NPTS2))
     call brsac(100,NPTS1,nmz1,dz1,nerr)
     call brsac(100,NPTS2,nmz2,dz2,nerr)
     call brsac(100,NPTS1,nmr1,dr1,nerr)
     call brsac(100,NPTS2,nmr2,dr2,nerr)
     call brsac(100,NPTS1,nmt1,dt1,nerr)
     call brsac(100,NPTS2,nmt2,dt2,nerr)
     if(f0==1) then                         !��ͨ�˲�
      call bpfn(dz1,NPTS1,DT,1/f4,1/f3,1/f2,1/f1)
      call bpfn(dz2,NPTS2,DT,1/f4,1/f3,1/f2,1/f1)
      call bpfn(dr1,NPTS1,DT,1/f4,1/f3,1/f2,1/f1)
      call bpfn(dr2,NPTS2,DT,1/f4,1/f3,1/f2,1/f1)
      call bpfn(dt1,NPTS1,DT,1/f4,1/f3,1/f2,1/f1)
      call bpfn(dt2,NPTS2,DT,1/f4,1/f3,1/f2,1/f1)
     endif
     !����P����ʱ��
     if(iphase/=2.and.P1/=-12345.0e0.and.P2/=-12345.0e0) then
      !����ȷ��ʱ��������,��ֹP��S�������
      slidel1=sld1
      slider1=sld1
      slidel2=sld2
      slider2=sld2
      if((0.73*(P1-O1)-pwr)>0.0e0) then
       slider1=min(slider1,0.73*(P1-O1)-pwr)
       slider2=min(slider2,0.73*(P1-O1)-pwr)
      else
       slider1=0.0e0
       slider2=0.0e0
      endif
      !���ļ�
      wln1=nint((P1+pwl-B1)/DT)+1  !ȷ����ʼʱ��:P
      wrn1=nint((P1+pwr-B1)/DT)+1
      !wln11=nint((P1+swl-B1)/DT)+1  !�ڶ���ʼʱ��:P
      !wrn11=nint((P1+swr-B1)/DT)+1
      wn=abs(wrn1-wln1+1)
      !wnn=abs(wrn11-wln11+1)
      if(slidewin==2) then !���ļ��͸��ļ������û�����
       sln1=nint((P1+pwl-slidel1-B1)/DT)+1
       srn1=nint((P1+pwr+slider1-B1)/DT)+1  !��һ�������߽�
       sln11=nint((P1+pwl-slidel2-B1)/DT)+1
       srn11=nint((P1+pwr+slider2-B1)/DT)+1  !�ڶ��������߽�
      else
       sln1=wln1
       srn1=wrn1
       sln11=wln1
       srn11=wrn1
      endif
      if(srn1>NPTS1) srn1=NPTS1        !�жϱ߽��Ƿ񳬳����ݷ�Χ
      if(sln1<1) sln1=1
      if(srn1==NPTS1.or.sln1==1) then
       print*, 'The P-window is larger than the data of ',trim(name1),', please check!'
       !write(10,*) 'The P-window is larger than the data of ',trim(name1),', please check!'
       !cycle
      endif
      if(srn11>NPTS1) srn11=NPTS1
      if(sln11<1) sln11=1
      if(srn11==NPTS1.or.sln11==1) then
       print*, 'The P_2-window is larger than the data of ',trim(name1),', please check!'
       !write(10,*) 'The P_2-window is larger than the data of ',trim(name1),', please check!'
       !cycle
      endif
      !���ļ�
      wln2=nint((P2+pwl-B2)/DT)+1
      wrn2=wln2+wn-1
      !wln22=nint((P2+swl-B2)/DT)+1
      !wrn22=wln22+wnn-1
      slidel1=sld1
      slider1=sld1
      slidel2=sld2
      slider2=sld2
      if((0.73*(P2-O2)-pwr)>0.0e0) then
       slider1=min(slider1,0.73*(P2-O2)-pwr)
       slider2=min(slider2,0.73*(P2-O2)-pwr)
      else
       slider1=0.0e0
       slider2=0.0e0
      endif
      sln2=nint((P2+pwl-slidel1-B2)/DT)+1  !�жϱ߽��Ƿ񳬳����ݷ�Χ
      srn2=nint((P2+pwr+slider1-B2)/DT)+1  !��һʱ���߽�
      sln22=nint((P2+pwl-slidel2-B2)/DT)+1  !�жϱ߽��Ƿ񳬳����ݷ�Χ
      srn22=nint((P2+pwr+slider2-B2)/DT)+1  !�ڶ�ʱ���߽�
      if(srn2>NPTS2) srn2=NPTS2
      if(sln2<1) sln2=1
      if(srn2==NPTS2.or.sln2==1) then
       print*, 'The P-window is larger than the data of ',trim(name2),', please check!'
       !write(10,*) 'The P-window is larger than the data of ',trim(name2),', please check!'
       !cycle
      endif
      if(srn22>NPTS2) srn22=NPTS2
      if(sln22<1) sln22=1
      if(srn22==NPTS2.or.sln22==1) then
       print*, 'The P_2-window is larger than the data of ',trim(name2),', please check!'
       !write(10,*) 'The P_2-window is larger than the data of ',trim(name2),', please check!'
       !cycle
      endif
      !ʱ����ػ���غ�������
      allocate(cr1(wn),ct1(wn),cz1(wn),cr2(wn),ct2(wn),cz2(wn))
      !allocate(cr11(wnn),ct11(wnn),cz11(wnn),cr22(wnn),ct22(wnn),cz22(wnn))
      !allocate(cr12(-wn+1:wn-1),ct12(-wn+1:wn-1),cz12(-wn+1:wn-1),c12(-wn+1:wn-1))
      !allocate(cr1122(-wnn+1:wnn-1),ct1122(-wnn+1:wnn-1),cz1122(-wnn+1:wnn-1),c1122(-wnn+1:wnn-1))
      cmax1=-99999.0e0
      cmax2=-99999.0e0
      k1=sln11            !ZR��������P�����������
      do while(k1+wn-1<=srn11)
       cr1(1:wn)=dr1(k1:k1+wn-1)
       !ct1(1:wn)=dt1(k1:k1+wn-1)
       cz1(1:wn)=dz1(k1:k1+wn-1)
       call nor(cr1,wn)
       !call nor(ct11,wnn)
       call nor(cz1,wn)
       k2=sln22
       do while(k2+wn-1<=srn22)                  !��������
        cr2(1:wn)=dr2(k2:k2+wn-1)
        !ct2(1:wn)=dt2(k2:k2+wn-1)
        cz2(1:wn)=dz2(k2:k2+wn-1)
        call nor(cr2,wn)
        !call nor(ct2,wn)
        call nor(cz2,wn)
        aa=0.0e0
        bb=0.0e0
        do j=1,wn
         aa=aa+cr1(j)*cr2(j)
        enddo
        do j=1,wn
         bb=bb+cz1(j)*cz2(j)
        enddo
        cc=aa+bb
        aa=0.0e0
        bb=0.0e0
        do j=1,wn
         aa=aa+cr1(j)*cr1(j)
        enddo
        do j=1,wn
         bb=bb+cr2(j)*cr2(j)
        enddo
        cr=sqrt(aa*bb)
        aa=0.0e0
        bb=0.0e0
        do j=1,wn
         aa=aa+cz1(j)*cz1(j)
        enddo
        do j=1,wn
         bb=bb+cz2(j)*cz2(j)
        enddo
        cz=sqrt(aa*bb)
        cc=cc/(cz+cr)
        if(cmax2<cc) then
         cmax2=cc
         cn11=k1
         cn22=k2
        endif
        if(k1>=sln1.and.k1+wn-1<=srn1.and.k2>=sln2.and.k2+wn-1<=srn2) then
         if(cmax1<cc) then
          cmax1=cc
          cn1=k1
          cn2=k2
         endif
        endif
        k2=k2+1
       enddo
       k1=k1+1
      enddo
      odt=(P1-O1)-(P2-O2)         !��ʼ��λ��������ʱ��
      sdt1=(wln1-cn1)*DT
      sdt2=(wln2-cn2)*DT          !�ƶ�������������ʱ���,����Ϊ��,����Ϊ��
      sdt11=(wln1-cn11)*DT
      sdt22=(wln2-cn22)*DT
      cdt1=sdt2-sdt1
      cdt2=sdt22-sdt11            !��ظ�����
      pdt1=odt+cdt1
      pdt2=odt+cdt2               !�������ʱ��tt1-tt2
      !P1=P1-sdt1
      !P2=P2-sdt2-tdt
      !if(slidewin==2) then
       !if(ID1<ID2) then
        !write(10,*) 'P:',cmax1,odt,sdt1,sdt2,cdt1,pdt1
        !write(10,*) 'P:',cmax2,odt,sdt11,sdt22,cdt2,pdt2
       !else
        !write(10,*) 'P:',cmax1,-odt,sdt2,sdt1,-cdt1,-pdt1
        !write(10,*) 'P:',cmax2,-odt,sdt22,sdt11,-cdt2,-pdt2
       !endif
      !else
       !if(ID1<ID2) then
        !write(10,*) 'P:',cmax1,odt,cdt1,pdt1
        !write(10,*) 'P:',cmax2,odt,cdt2,pdt2
       !else
        !write(10,*) 'P:',cmax1,-odt,-cdt1,-pdt1
        !write(10,*) 'P:',cmax2,-odt,-cdt2,-pdt2
       !endif
      !endif
      if(cmax1>=ccut.or.cmax2>=ccut) then  !ѡ�����Ƶ�����
       if(ID1<ID2) then
        write(13,200) ID1,ID2,d12,name1(mm+1:nn-1),pdt1,cdt1,cmax1,pdt2,cdt2,cmax2,'P  ',  &
                   &  dist1,dist2,az1,az2
       else
        write(13,200) ID2,ID1,d12,name1(mm+1:nn-1),-pdt1,-cdt1,cmax1,-pdt2,-cdt2,cmax2,'P  ',  &
                   &  dist2,dist1,az2,az1
       endif
      endif
      200 format(2i8,1x,f7.3,1x,a,2(1x,f8.3,1x,f6.3,1x,f7.4),1x,a,4(1x,f8.3))
      deallocate(cr1,ct1,cz1,cr2,ct2,cz2)
      !deallocate(cr11,ct11,cz11,cr22,ct22,cz22,cr1122,ct1122,cz1122,c1122)
     endif
     !����S����ʱ��
     if(iphase/=1.and.S1/=-12345.0e0.and.S2/=-12345.0e0) then
      !����ȷ��ʱ��������,��ֹP��S�������
      slidel1=sld1
      slider1=sld1
      slidel2=sld2
      slider2=sld2
      if((0.73*(P1-O1)+swl)>0.0e0) then
       slidel1=min(slidel1,0.73*(P1-O1)+swl)
       slidel2=min(slidel2,0.73*(P1-O1)+swl)
      else
       slidel1=0.0e0
       slidel2=0.0e0
      endif
      !���ļ�
      wln1=nint((S1+swl-B1)/DT)+1  !ȷ����ʼʱ��:S
      wrn1=nint((S1+swr-B1)/DT)+1  
      !wln11=nint((S1+swl-B1)/DT)+1  !�ڶ���ʼʱ��:S
      !wrn11=nint((S1+swr-B1)/DT)+1  
      wn=abs(wrn1-wln1+1)
      !wnn=abs(wrn11-wln11+1)
      if(slidewin==2) then  !���ļ��͸��ļ������û�����
       sln1=nint((S1+swl-slidel1-B1)/DT)+1
       srn1=nint((S1+swr+slider1-B1)/DT)+1  !��һʱ���߽�
       sln11=nint((S1+swl-slidel2-B1)/DT)+1
       srn11=nint((S1+swr+slider2-B1)/DT)+1  !�ڶ�ʱ���߽�
      else
       sln1=wln1
       srn1=wrn1
       sln11=wln1
       srn11=wrn1
      endif
      if(srn1>NPTS1) srn1=NPTS1   !�жϱ߽��Ƿ񳬳����ݷ�Χ
      if(sln1<1) sln1=1
      if(srn1==NPTS1.or.sln1==1) then
       print*, 'The S-window is larger than the data of ',trim(name1),', please check!'
       !write(10,*) 'The S-window is larger than the data of ',trim(name1),', please check!'
       !cycle
      endif
      if(srn11>NPTS1) srn11=NPTS1
      if(sln11<1) sln11=1
      if(srn11==NPTS1.or.sln11==1) then
       print*, 'The S_2-window is larger than the data of ',trim(name1),', please check!'
       !write(10,*) 'The S_2-window is larger than the data of ',trim(name1),', please check!'
       !cycle
      endif
      !���ļ�
      wln2=nint((S2+swl-B2)/DT)+1
      wrn2=wln2+wn-1
      !wln22=nint((S2+swl-B2)/DT)+1
      !wrn22=wln22+wnn-1
      slidel1=sld1
      slider1=sld1
      slidel2=sld2
      slider2=sld2
      if((0.73*(P2-O2)+swl)>0.0e0) then
       slidel1=min(slidel1,0.73*(P2-O2)+swl)
       slidel2=min(slidel2,0.73*(P2-O2)+swl)
      else
       slidel1=0.0e0
       slidel2=0.0e0
      endif
      sln2=nint((S2+swl-slidel1-B2)/DT)+1
      srn2=nint((S2+swr+slider1-B2)/DT)+1  !��һʱ���߽�
      sln22=nint((S2+swl-slidel2-B2)/DT)+1
      srn22=nint((S2+swr+slider2-B2)/DT)+1  !�ڶ�ʱ���߽�
      if(srn2>NPTS2) srn2=NPTS2        !�жϱ߽��Ƿ񳬳����ݷ�Χ
      if(sln2<1) sln2=1
      if(srn2==NPTS2.or.sln2==1) then
       print*, 'The S-window is larger than the data of ',trim(name2),', please check!'
       !write(10,*) 'The S-window is larger than the data of ',trim(name2),', please check!'
       !cycle
      endif
      if(srn22>NPTS2) srn22=NPTS2
      if(sln22<1) sln22=1
      if(srn22==NPTS2.or.sln22==1) then
       print*, 'The S_2-window is larger than the data of ',trim(name2),', please check!'
       !write(10,*) 'The S_2-window is larger than the data of ',trim(name2),', please check!'
       !cycle
      endif
      !����ؼ���
      allocate(cr1(wn),ct1(wn),cz1(wn),cr2(wn),ct2(wn),cz2(wn))
      !allocate(cr11(wnn),ct11(wnn),cz11(wnn),cr22(wnn),ct22(wnn),cz22(wnn))
      !allocate(cr12(-wn+1:wn-1),ct12(-wn+1:wn-1),cz12(-wn+1:wn-1),c12(-wn+1:wn-1))
      !allocate(cr1122(-wnn+1:wnn-1),ct1122(-wnn+1:wnn-1),cz1122(-wnn+1:wnn-1),c1122(-wnn+1:wnn-1))
      cmax1=-99999.0e0
      cmax2=-99999.0e0
      k1=sln11        !RT��������P�����������
      do while(k1+wn-1<=srn11)
       cr1(1:wn)=dr1(k1:k1+wn-1)
       ct1(1:wn)=dt1(k1:k1+wn-1)
       !cz1(1:wn)=dz1(k1:k1+wn-1)
       call nor(cr1,wn)
       call nor(ct1,wn)
       !call nor(cz1,wn)
       k2=sln22
       do while(k2+wn-1<=srn22)                  !��������
        cr2(1:wn)=dr2(k2:k2+wn-1)
        ct2(1:wn)=dt2(k2:k2+wn-1)
        !cz2(1:wn)=dz2(k2:k2+wn-1)
        call nor(cr2,wn)
        call nor(ct2,wn)
        !call nor(cz2,wn)
        aa=0.0e0
        bb=0.0e0
        do j=1,wn
         aa=aa+cr1(j)*cr2(j)
        enddo
        do j=1,wn
         bb=bb+ct1(j)*ct2(j)
        enddo
        cc=aa+bb
        aa=0.0e0
        bb=0.0e0
        do j=1,wn
         aa=aa+cr1(j)*cr1(j)
        enddo
        do j=1,wn
         bb=bb+cr2(j)*cr2(j)
        enddo
        cr=sqrt(aa*bb)
        aa=0.0e0
        bb=0.0e0
        do j=1,wn
         aa=aa+ct1(j)*ct1(j)
        enddo
        do j=1,wn
         bb=bb+ct2(j)*ct2(j)
        enddo
        ct=sqrt(aa*bb)
        cc=cc/(cz+cr)
        if(cmax2<cc) then
         cmax2=cc
         cn11=k1
         cn22=k2
        endif
        if(k1>=sln1.and.k1+wn-1<=srn1.and.k2>=sln2.and.k2+wn-1<=srn2) then
         if(cmax1<cc) then
          cmax1=cc
          cn1=k1
          cn2=k2
         endif
        endif
        k2=k2+1
       enddo
       k1=k1+1
      enddo
      odt=(S1-O1)-(S2-O2)         !��ʼ��λ��������ʱ��
      sdt1=(wln1-cn1)*DT
      sdt2=(wln2-cn2)*DT          !�ƶ�������������ʱ���,����Ϊ��,����Ϊ��
      sdt11=(wln1-cn11)*DT
      sdt22=(wln2-cn22)*DT
      cdt1=sdt2-sdt1
      cdt2=sdt22-sdt11            !��ظ�����
      pdt1=odt+cdt1
      pdt2=odt+cdt2               !�������ʱ��tt1-tt2
      !S1=S1-sdt1
      !S2=S2-sdt2-tdt
      !if(ID1<ID2) then
       !write(10,*) 'S:',cmax1,odt,sdt1,sdt2,cdt1,pdt1
       !write(10,*) 'S:',cmax2,odt,sdt11,sdt22,cdt2,pdt2
      !else
       !write(10,*) 'S:',cmax1,-odt,sdt2,sdt1,-cdt1,-pdt1
       !write(10,*) 'S:',cmax2,-odt,sdt22,sdt11,-cdt2,-pdt2
      !endif
      if(cmax1>=ccut.or.cmax2>=ccut) then  !ѡ�����Ƶ�����
       if(ID1<ID2) then
        write(13,202) ID1,ID2,d12,name1(mm+1:nn-1),pdt1,cdt1,cmax1,pdt2,cdt2,cmax2,'S  ',  &
                   &  dist1,dist2,az1,az2
       else
        write(13,202) ID2,ID1,d12,name1(mm+1:nn-1),-pdt1,-cdt1,cmax1,-pdt2,-cdt2,cmax2,'S  ',  &
                   &  dist2,dist1,az2,az1
       endif
      endif
      202 format(2i8,1x,f7.3,1x,a,2(1x,f8.3,1x,f6.3,1x,f7.4),1x,a,4(1x,f8.3))
      deallocate(cr1,ct1,cz1,cr2,ct2,cz2)
      !deallocate(cr11,ct11,cz11,cr22,ct22,cz22,cr1122,ct1122,cz1122,c1122)
     endif
     deallocate(dz1,dr1,dt1,dz2,dr2,dt2)
     exit
    endif
   enddo
  enddo
  close(13)
 enddo
 !deallocate(eve,sta,pos)
 deallocate(id,sta,pos,epid,epdist)
 !close(10)
 !close(12)


 99 continue
 call CPU_TIME(T2)
 print*, 'T=',T2-T1
end program


subroutine exist(fn)
 implicit none
 character*(*)::fn
 logical::ex
 inquire(file=fn,exist=ex)
 if(.not.ex) then
  write(*,'("file does not exist ",a)') trim(fn)
  stop
 endif
 return
end subroutine


subroutine filelen(fileid,n)
!�õ��ļ�����
 implicit none
 integer*4::fileid,n
 rewind(fileid)
 n=0
 do
  read(fileid,*,end=20)
  n=n+1
 enddo
 20 rewind(fileid)
 return
end subroutine


subroutine brsach(IRU,name,nerr)
!-----
!       IRU I*4 logical unit for IO
!       name    C*  Name of file to be opened
!       rhdr    R*4 Real header
!       ihdr    I*4 Integer Header
!       chdr    C*  Character Header
!       nerr    I*4 -1 file does not exist
!               -2 data points in file exceed dimension
!
!       NOTE IF THE BINARY FILE HAS MORE THAN LN POINTS, THEN ONLY
!       LN POINTS ARE USED
!-----
!  This routine reads waveform data written in SAC binary format.
!
!  Written by Hafidh A. A. Ghalib, 1988.
!-----
 implicit none
 integer*4::i,IRU,nerr
 logical::ext
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)
 character*(*)::name
!-----
!  Read real and integer header blocks to find actual number
!  of waveform data points which is stored in ihdr(10).
!-----
 inquire(file=name,exist=ext)
 if(.not.ext) then
  ihdr(10)=0
  nerr=-1
  return
 endif
 nerr=0
 !open(IRU,file=name,form='unformatted',access='direct',recl=440,status='old')
 !read(IRU,rec=1) (rhdr(i),i=1,70),(ihdr(i),i=1,40)
 open(IRU,file=name,form='unformatted',access='direct',recl=632,status='old')
 read(IRU,rec=1) (rhdr(i),i=1,70),(ihdr(i),i=1,40),(chdr(i),i=1,24)
 close(IRU)
 return
end subroutine


subroutine brsac(IRU,LN,name,data,nerr)
!-----
!       IRU I*4 logical unit for IO
!       LN  I*4 length of data array
!       name    C*  Name of file to be opened
!       rhdr    R*4 Real header
!       ihdr    I*4 Integer Header
!       chdr    C*  Character Header
!       data    R*4 Array of trace values
!       nerr    I*4 -1 file does not exist
!               -2 data points in file exceed dimension
!
!       NOTE IF THE BINARY FILE HAS MORE THAN LN POINTS, THEN ONLY
!       LN POINTS ARE USED
!-----
!  Written by Hafidh A. A. Ghalib, 1988.
!
!  Modified by Wang, 2012
!-----
 implicit none
 integer*4::i,IRU,LN,nerr,maxpts,nbytes
 real*4::data(LN)
 logical::ext
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)
 character*(*)::name
!  Read real and integer header blocks to find actual number
!  of waveform data points which is stored in ihdr(10).
! print*, name
 inquire(file=name,exist=ext)
 if(.not.ext) then
  ihdr(10)=0
  nerr=-1
  return
 endif
 nerr=0
 open(IRU,file=name,form='unformatted',access='direct',recl=440,status='old')
 read(IRU,rec=1) (rhdr(i),i=1,70),(ihdr(i),i=1,40)
 close(IRU)
!  Read header and waveform data blocks using recored length of 158*4=632.
 if(ihdr(10).gt.LN) then
  maxpts=LN
  nerr=-2
 else
  maxpts=ihdr(10)
  nerr=0
 endif
 nbytes=632+4*maxpts
 open(IRU,file=name,form='unformatted',access='direct',recl=nbytes)
 read(IRU,rec=1)  (rhdr(i),i=1,70),   &
                  (ihdr(i),i=1,40),   &
                  (chdr(i),i=1,24),   &
                  (data(i),i=1,maxpts)
 close(IRU)
 !ihdr(10)=maxpts
 return
end subroutine


subroutine dot(str,n,i)
!Ѱ���ַ���str�еĵ�n��.�ŵ�λ��i
 implicit none
 integer*4::i,j,k,n
 character*1::flag
 character*(*)::str
 j=len_trim(str)
 k=0
 do i=1,j
  flag=str(i:i)
  if(flag=='.') then
   k=k+1
   if(k==n) exit
  endif
 enddo
 if(k==0) print*, 'No dot in:',str
 return
end subroutine


subroutine bpfn(data,n,dt,t1,t2,t3,t4)
 implicit none
 integer::i,n,logn,nfft
 real*4::dt,df,t1,t2,t3,t4,f1,f2,f3,f4,f,bp
 real*4::data(n)
 real*4,allocatable::PR(:),PI(:),FR(:),FI(:)
 real*4,parameter::pai=3.14159265358e0
 call npow2(n,nfft,logn)
 allocate(PR(nfft),PI(nfft),FR(nfft),FI(nfft))
 df=1.0e0/(nfft*dt)
 f1=1.0e0/t4
 f2=1.0e0/t3
 f3=1.0e0/t2
 f4=1.0e0/t1
 !print*, df,f1,f2,f3,f4
 PR=0.0e0
 PI=0.0e0
 FR=0.0e0
 FI=0.0e0
 PR(1:n)=data(1:n)
 call taper(PR,nfft,0.25)                !ƽ�����ݵ�����,���⼪��˹����
 call KKFFT(PR,PI,nfft,logn,FR,FI,0,0)
 do i=1,nfft/2+1
  f=(i-1)*df
  if(f.ge.f1.and.f.le.f2) then
   bp=0.5e0*(cos(pai*(f-f2)/(f2-f1))+1.0e0)
  elseif(f.gt.f2.and.f.lt.f3) then
   bp=1.0e0
  elseif(f.ge.f3.and.f.le.f4) then
   bp=0.5e0*(cos(pai*(f-f3)/(f4-f3))+1.0e0)
  else
   bp=0.0e0
  endif
  PR(i)=FR(i)*bp
  PI(i)=FI(i)*bp
 enddo
 PR(1)=PR(1)/2
 PI(1)=PI(1)/2
 PR(nfft/2+1)=PR(nfft/2+1)/2
 PI(nfft/2+1)=PI(nfft/2+1)/2
 PR(nfft/2+2:nfft)=0.0e0
 PI(nfft/2+2:nfft)=0.0e0       !�Ƶ��Ϊ��
 !call taper(PR,nfft,0.005)
 !call taper(PI,nfft,0.005)
 call KKFFT(PR,PI,nfft,logn,FR,FI,1,0)
 data(1:n)=2*FR(1:n)
 return
end subroutine


subroutine GetDistAz(lat1,long1,lat2,long2,dist,az)
!�����㾭γ�ȼ����Բ�ӻ����ͷ�λ��
!longitude:����;latitude:γ��
!����Ϊ��,����Ϊ��,��γΪ��,��γΪ��
!1Ϊ�۲��,2ΪĿ���,azΪ1���2�ķ�λ��;
!1Ϊ�¼�,2Ϊ̨վ, azΪ�¼���̨վ�ķ�λ��;
 implicit none
 real*4::lat1,long1,lat2,long2,dist,az
 real*4::radlat1,radlong1,radlat2,radlong2,a,b,radlat
 real*4,parameter::EARTH_RADIUS=6371.004e0  !ƽ���뾶
 !real*4,parameter::EARTH_RADIUS=6378.137e0  !����뾶
 real*4,parameter::pi=3.1415926535898e0
 radlat1=lat1*pi/180.0e0
 radlong1=long1*pi/180.0e0
 radlat2=lat2*pi/180.0e0
 radlong2=long2*pi/180.0e0
 a=radLat1-radLat2
 b=radlong1-radlong2
 radlat=(radlat1+radlat2)/2
 !dist=acos(sin(radlat1)*sin(radlat2)+cos(radlat1)*cos(radlat2)*cos(b))*EARTH_RADIUS
 dist=2*asin(sqrt(sin(a/2)*sin(a/2)+cos(radlat1)*cos(radlat2)*sin(b/2)*sin(b/2)))*EARTH_RADIUS
 if(lat1.eq.lat2) then
  az=90.0e0
 else
  az=atan(b/a*cos(radlat))*180.0e0/pi
 endif
 if(lat1.gt.lat2) then
  az=az+180.0e0
 endif
 if(az.lt.0.0e0) then
  az=az+360.0e0
 endif
 return
end subroutine


subroutine corcoe(x,y,n,cc)
!���������x(n),y(n)�������������ϵ��cc
 implicit none
 integer*4::n,i
 real*4::x(n),y(n),cc,a,b,aa,bb,ab
 a=0.0e0
 b=0.0e0
 aa=0.0e0
 bb=0.0e0
 ab=0.0e0
 do i=1,n
  a=a+x(i)
  b=b+y(i)
  aa=aa+x(i)*x(i)
  bb=bb+y(i)*y(i)
  ab=ab+x(i)*y(i)
 enddo
 cc=(ab-a*b/n)/sqrt((aa-a*a/n)*(bb-b*b/n))
 return
end subroutine


subroutine taper(data,n,width)
!����sac2000�е�taper����
!DATA(J)=DATA(J)*(F0-F1*COS(OMEGA*(J-1))
!======== ========= ===== ======
!TYPE     OMEGA     F0    F1
!======== ========= ===== ======
!HANNING   PI/N     0.50  0.50
!HAMMING   PI/N     0.54  0.46
!COSINE    PI/(2*N) 1.00  1.00
!======== ========= ===== ======
!����ѡ��HANNING
 implicit none
 integer*4::i,j,k,n,tl,tr
 real*4::data(n),width,f0,f1,omega
 real*4,parameter::pi=3.14159265358e0
 do i=1,n
  if(data(i).ne.0.0e0) exit
 enddo
 if(i.eq.n) then
  print*, 'This is a zero data, please check'
  return
 endif
 tl=i
 do i=n,1,-1
  if(data(i).ne.0.0e0) exit
 enddo
 tr=i
 f0=0.5e0
 f1=0.5e0
 j=anint((tr-tl+1)*width)
 if(j.eq.1.or.j.eq.0) return
 omega=pi/j
 do i=tl,tl+j-1
  data(i)=data(i)*(f0-f1*cos(omega*(i-tl)))
 enddo
 do i=tr,tr-j+1,-1
  data(i)=data(i)*(f0-f1*cos(omega*(i-tr)))
 enddo
 return
end subroutine


subroutine npow2(n,nfft,logn)
 implicit none
 integer*4::n,nfft,logn
 nfft=1
 logn=0
 do while(nfft.lt.n)
  nfft=2*nfft
  logn=logn+1
 enddo
 return
end subroutine


SUBROUTINE TD(A,NA,B,NB,DT,TDT,CC)
!ʱ������㻥��غ���,�õ�ʱ���TDT(��������ֵ��Ӧʱ��ƫ��)
!A(NA),B(NB):ʱ������;DT:�������;CC:��������ֵ;
 IMPLICIT NONE
 INTEGER*4::I,J,NA,NB
 REAL*4::DT,TDT,AA,BB,CC
 REAL*4::A(NA),B(NB),C(-NA+1:NB-1)
!ʱ���㷨
 !CALL NOR(A,NA)
 !CALL NOR(B,NB)
 CALL TCOR(A,NA,B,NB,C,-NA+1,NB-1)  !ʱ���м��㻥��غ���
 AA=0.0E0
 BB=0.0E0
 !PRINT*, 'A(1)= ',A(1)
 !PRINT*, 'B(1)= ',B(1)
 DO I=1,NA
  AA=AA+A(I)*A(I)
 ENDDO
 DO I=1,NB
  BB=BB+B(I)*B(I)
 ENDDO
 C=C/SQRT(AA*BB)
 CC=0.0E0
 J=0
 DO I=-NA+1,NB-1
  IF(CC.LT.C(I)) THEN
   CC=C(I)
   J=I
  ENDIF
 ENDDO
 !PRINT*, 'J= ',J,'CF= ',CC
 TDT=-J*DT
 RETURN
END SUBROUTINE


SUBROUTINE KKFFT(PR,PI,N,K,FR,FI,L,IL)
!L=0 for fft;L=1 for ifft
!IL=0 ������ģ�����;IL=1 ����ģ�����
 IMPLICIT NONE
 INTEGER*4::I,N,K,IT,L,IL,M,IS,J,NV,L0
 REAL*4::PR(N),PI(N),FR(N),FI(N),P,Q,S,VR,VI,PODDR,PODDI
 DO IT=0,N-1
  M=IT
	IS=0
	DO I=0,K-1
	 J=M/2
	 IS=2*IS+(M-2*J)
	 M=J
  ENDDO
	FR(IT+1)=PR(IS+1)
	FI(IT+1)=PI(IS+1)
 ENDDO
 PR(1)=1.0E0
 PI(1)=0.0E0
 PR(2)=COS(6.283185306E0/N)
 PI(2)=-SIN(6.283185306E0/N)
 IF(L.NE.0) PI(2)=-PI(2)
 DO I=3,N
	P=PR(I-1)*PR(2)
	Q=PI(I-1)*PI(2)
	S=(PR(I-1)+PI(I-1))*(PR(2)+PI(2))
	PR(I)=P-Q
	PI(I)=S-P-Q
 ENDDO
 DO IT=0,N-2,2
	VR=FR(IT+1)
	VI=FI(IT+1)
	FR(IT+1)=VR+FR(IT+2)
	FI(IT+1)=VI+FI(IT+2)
	FR(IT+2)=VR-FR(IT+2)
	FI(IT+2)=VI-FI(IT+2)
 ENDDO
 M=N/2
 NV=2
 DO L0=K-2,0,-1
	M=M/2
	NV=2*NV
	DO IT=0,(M-1)*NV,NV
	 DO J=0,(NV/2)-1
	  P=PR(M*J+1)*FR(IT+J+1+NV/2)
	  Q=PI(M*J+1)*FI(IT+J+1+NV/2)
	  S=PR(M*J+1)+PI(M*J+1)
	  S=S*(FR(IT+J+1+NV/2)+FI(IT+J+1+NV/2))
	  PODDR=P-Q
	  PODDI=S-P-Q
	  FR(IT+J+1+NV/2)=FR(IT+J+1)-PODDR
    FI(IT+J+1+NV/2)=FI(IT+J+1)-PODDI
	  FR(IT+J+1)=FR(IT+J+1)+PODDR
	  FI(IT+J+1)=FI(IT+J+1)+PODDI
   ENDDO
  ENDDO
 ENDDO
 IF(L.NE.0) THEN
  DO I=1,N
	 FR(I)=FR(I)/N
	 FI(I)=FI(I)/N
  ENDDO
 ENDIF
 IF(IL.NE.0) THEN
  DO I=1,N
	 PR(I)=SQRT(FR(I)*FR(I)+FI(I)*FI(I))
   PI(I)=ATAN(FI(I)/FR(I))*360.0E0/6.283185306E0
   !����λ��ԭΪ[0,2pai],��[0,360]
   IF(FR(I).LT.0.0E0) PI(I)=180.0E0+PI(I)
   IF(FI(I).LE.0E0.AND.FR(I).GE.0.0E0) PI(I)=360.0E0+PI(I)
  ENDDO
 ENDIF
 RETURN
END SUBROUTINE


SUBROUTINE TCOR(X,M,H,N,Y,LN,RN)
 IMPLICIT NONE
! This program is purposed to calculate the auto-correlation or
! corss-correlation function in time-domain. Creatied: 2010/12/17 15:25:44
 INTEGER*4::I,J,M,N,LN,RN
 REAL*4::X(M),H(N),Y(LN:RN)
! X(M),H(N)Ϊ���룬Y(-M+1:N-1)Ϊ�����YΪX��H��������У���M+N-1��
! Y(0)  ΪX(M),H(N)�������ʱ�����ֵ��
! Y(1)  ΪH(N)���X(M)����һ��ʱ�����ֵ�������֧��N-1�
! Y(-1) ΪH(N)���X(M)����һ��ʱ�����ֵ�������֧��M-1�
! LN��ʾ�����������е���߽磬RN��ʾ�����������е��ұ߽硣
 IF(LN.LT.-M+1) PRINT*, '��߽糬��'
 IF(RN.GT.N-1) PRINT*, '�ұ߽糬��'
 DO I=LN,RN
  Y(I)=0.0e0
  DO J=1,M
   IF(J+I.GE.1.AND.J+I.LE.N) Y(I)=Y(I)+X(J)*H(J+I)
  ENDDO
 ENDDO
 RETURN
END SUBROUTINE


subroutine nor(data,n)
 implicit none
 integer*4::n,i
 real*4::data(n),max
 max=0.0e0
 do i=1,n
  if(max.lt.abs(data(i))) max=abs(data(i))
 enddo
 data=data/max
end subroutine


subroutine namegen(prename,num,index,ext,name)
! *****************************************************************
! generate sorted file name. attach the number after each filename.
! the conversion of integer value into character is used.
!
! input:
! prename The prefix filename;
! num indicate the maximum file number used, e.g."0000","00000";
! index the number of this file;
! ext extension of the file;
! example:
! name='file'
! num='0000'
! index=18
! ext='txt'
! returned name is: file0018.txt
! *****************************************************************
 implicit none
 character*100::na,num1
 character*(*)::prename,name,ext,num
 integer*4::index,length1,length2
 write(na,'(i20)') index
 na=adjustl(na)
 length1=len_trim(na)
 length2=len_trim(num)
 if(length2>length1) then
  num1=num(1:(length2-length1))//na
 else
  num1=na
 endif
 name=trim(prename)//trim(num1)//'.'//trim(ext)
end subroutine


