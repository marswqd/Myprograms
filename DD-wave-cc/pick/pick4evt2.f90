!������Ĺ�����:���ݵ���Ŀ¼�������¼����������μ�¼�н�ȡ����,����ʱ�䷶Χ:����ʱ��-���о�/1
!               ���������̨վ�ľ�γ��,���о�,����ʱ��,������,������Ϣ��д����sac�ļ�
!  ע: ������ֻ��ȡ�ڵ���Ŀ¼���б궨��̨վ����Ӧʱ�β�������.
!      ������ͨ����ȡsac�ļ�����ȡ̨վ���ͷ�����,
!            ����sac�ļ���������ʽ��Ϊ�ض���ʽ,�� 20120907084203.Kun.HWS.BHE
!            ���ڶ�ȡ����̨����evt2sac�õ���sac�ļ�
!            ���sac�ļ���������ʽΪ HWS.20120907111943.E.SAC
!            ���޳��������˵�0ֵ
!����: ���嶫  ʱ��: 2015/8/12 17:01:21
program pick4evt2
 implicit none
 
 integer*4::i,j,j1,k,m,n,nn,nm,p,nerr,num,nsta,npos,neve,head,tail,ahead,atail,ID
 integer*4::re_year,re_days,year,month,day,hour,min,nsec,nmsec,days,meve,mpha
 integer*4,allocatable::NPTS(:),pos(:),npha(:)
 integer*4,allocatable::iyear(:),imonth(:),iday(:),ihour(:),imin(:),iID(:)
 real*4::T1,T2,sec,stla,stlo,evla,evlo,depth,mag,tt,dist,az,weight
 real*4,allocatable::rsec(:),revla(:),revlo(:),rdepth(:),rmag(:),rtt(:,:),rweight(:,:)
 real*8::DT,t,bm,em,O,F   !Ϊ���־���,��ֹ������С��,�������е�ʱ�����������Ϊ˫����
 real*8,allocatable::B(:),E(:)
 character::flag*1,phase*1,tempc2*1,tempc1*5
 character*60::name,tname,wname,catalog,command
 character,allocatable::com(:)*1,sta(:)*5,csta(:,:)*5,cphase(:,:)*1
 character*60,allocatable::sacname(:)

 real*4,allocatable::data(:),adata(:),bdata(:)
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)


 call CPU_TIME(T1)

 open(10,file='pick.log')
 !re_year=2014  !�ӽ��ļ�������,������ֶ������
 !re_days=1     !����sac�ļ��͵���Ŀ¼���ܵĲο�ʱ��

!��ȡ�����ļ���Ϣ 
 open(11,file='pick.in',status='old')
 read(11,*) catalog                       !����Ŀ¼
 read(11,*) num,re_year,re_days
 allocate(sacname(num),sta(num),com(num),pos(num))
 do i=1,num
  read(11,*) sacname(i)
  call exist(sacname(i))
  sacname(i)=adjustl(sacname(i))
  print*, trim(sacname(i))
 enddo
 close(11)
 print*


 print*, 'Step 1'
 !Ѱ��̨վ.������
 open(12,file='station.txt')
 nsta=1
 k=1
 name=sacname(1)
 call dot(name,2,m)
 call dot(name,3,n)
 sta(1)=name(m+1:n-1)
 !call brsach(100,name,nerr)
 !sta(1)=chdr(1)
 !call isnul(sta(1))
 call upper(sta(1))
 write(12,*) trim(sta(1)),rhdr(32),rhdr(33),rhdr(34) !̨վ γ�� ���� �߳�
 com(1)=name(n+3:n+3)
 !com(1)=chdr(21)(3:3)   !kcmpnm=chdr(21)
 call upper(com(1))
 i=2
 22 do while(i<=num)
  name=sacname(i)
  i=i+1
  call dot(name,2,m)
  call dot(name,3,n)
  tempc1=name(m+1:n-1)
  !call brsach(100,name,nerr)
  !tempc1=chdr(1)
  !call isnul(tempc1)
  call upper(tempc1)
  tempc2=name(n+3:n+3)
  !tempc2=chdr(21)(3:3)
  call upper(tempc2)
  m=1         !Ѱ��̨վ,����̨վ��Ϣ�ļ�
  do j=1,nsta
   if(tempc1==sta(j)) then
    m=0
    exit
   endif
  enddo
  if(m==1) then
   k=k+1
   write(12,*) trim(tempc1),rhdr(32),rhdr(33),rhdr(34) !̨վ γ�� ���� �߳�
  endif
  do j=1,nsta
   if(tempc1==sta(j).and.tempc2==com(j)) goto 22
  enddo
  nsta=nsta+1
  sta(nsta)=tempc1
  com(nsta)=tempc2
 enddo
 close(12)
 write(10,*) 'There are ',k,' stations in the SAC files, see the file station.txt.'
 write(10,*) 'There are ',nsta,' station.component in the SAC files:'
 do i=1,nsta
  write(10,*) trim(sta(i))//'.'//com(i)
 enddo
 write(10,*) '-----'


 print*, 'Step 2'
 !������Ŀ¼,���䱣�����ڴ���
 open(13,file=catalog,status='old',action='read')
 i=0
 j=0
 do
  read(13,*,end=25) flag
  if(flag=='#') then  !�ҵ��¼���ʾ
   i=i+1
   k=0
   do                 !�ҵ��¼���Ӧ��������
    read(13,*,end=26) flag
    if(flag=='#') then
     backspace(13)
     exit
    endif
    k=k+1
   enddo
   26 continue
   backspace(13)
   if(j<k) j=k
  endif
 enddo
 25 continue
 meve=i
 mpha=j
 allocate(npha(meve))
 allocate(iyear(meve),imonth(meve),iday(meve),ihour(meve),imin(meve),iID(meve))
 allocate(rsec(meve),revla(meve),revlo(meve),rdepth(meve),rmag(meve))
 allocate(csta(meve,mpha),rtt(meve,mpha),rweight(meve,mpha),cphase(meve,mpha))
 rewind(13)
 i=0
 do
  read(13,*,end=27) flag
  if(flag=='#') then  !�ҵ��¼���ʾ
   i=i+1
   backspace(13)
   read(13,*) flag,iyear(i),imonth(i),iday(i),ihour(i),imin(i),rsec(i),   &
              revla(i),revlo(i),rdepth(i),rmag(i),tt,tt,tt,iID(i)
   k=0
   do                 !�ҵ��¼���Ӧ��������
    read(13,*,end=28) flag
    if(flag=='#') exit
    k=k+1
   enddo
   28 continue
   npha(i)=k      !��i���¼���npha(i)�������¼,������pha(i,npha(i))��
   do j=1,k+1
    backspace(13)
   enddo
   do j=1,k
    read(13,*) csta(i,j),rtt(i,j),rweight(i,j),cphase(i,j)
   enddo
  endif
 enddo
 27 continue
 close(13)


 print*, 'Step 3'
 !open(13,file=catalog,status='old',action='read')
 !��ȡ����,�����¼�SAC�ļ�
 do i=1,nsta
  print*, 'Beign with: ',trim(sta(i))//'.'//com(i)
  write(10,*) 'Beign with: ',trim(sta(i))//'.'//com(i)
  !�ҵ���̨ͬվ.������sacname�е�λ��
  npos=0
  do j=1,num
   name=sacname(j)
   call dot(name,2,m)
   call dot(name,3,n)
   tempc1=name(m+1:n-1)
   !call brsach(100,name,nerr)
   !tempc1=chdr(1)
   !call isnul(tempc1)
   call upper(tempc1)
   tempc2=name(n+3:n+3)
   !tempc2=chdr(21)(3:3)
   call upper(tempc2)
   if(tempc1==sta(i).and.tempc2==com(i)) then
    npos=npos+1
    pos(npos)=j
   endif
  enddo
  !����̨ͬվ.������sacͷ�ļ�,�õ�ʱ�䷶Χ
  allocate(B(npos),E(npos),NPTS(npos))
  do j=1,npos
   k=pos(j)
! DT=rhdr(1)     B=rhdr(6)      O=rhdr(8)      DIST=rhdr(51)
! A=rhdr(9)      T0=rhdr(11)    T1=rhdr(12)    T2=rhdr(13)
! STLA=rhdr(32)  STLO=rhdr(33)  STEL=rhdr(34)  STDP=rhdr(35)
! EVLA=rhdr(36)  EVLO=rhdr(37)  EVEL=rhdr(38)  EVDP=rhdr(39)
! NZYEAR=ihdr(1) NZJDAY=ihdr(2) NZHOUR=ihdr(3) NZMIN=ihdr(4)
! NZSEC=ihdr(5)  NZMSEC=ihdr(6) NPTS=ihdr(10)  KSTNM=chdr(1)
   call brsach(100,sacname(k),nerr)
   DT=rhdr(1)*1.0d0
   call ndays2(re_year,re_days,ihdr(1),ihdr(2),n)
   t=((n*24.0d0+ihdr(3)+8)*60.0d0+ihdr(4))*60.0d0+ihdr(5)*1.0d0+ihdr(6)*0.001d0
   !t=((((ihdr(1)-re_year)*365.0d0+ihdr(2)-re_days)*24.0d0+ihdr(3)+8)*60.0d0+   &
   !      ihdr(4))*60.0d0+ihdr(5)*1.0d0+ihdr(6)*0.001d0
   !t:��һ�ļ��ο�ʱ��������ܲο�ʱ���ƫ������,��λs;
   !ihdr(3)+8:Evt�ļ��е�ʱ��(��8��)=Sac�ļ��еĲο�ʱ��(GMT)+8��Сʱ
   B(j)=t+rhdr(6)*1.0d0
   E(j)=t+rhdr(7)*1.0d0
   NPTS(j)=ihdr(10)
  enddo
  bm=minval(B)
  em=maxval(E)
  nm=maxval(NPTS)
  allocate(adata(nm))
  !������Ŀ¼
  neve=0
  do j1=1,meve
   year=iyear(j1)
   month=imonth(j1)
   day=iday(j1)
   hour=ihour(j1)
   min=imin(j1)
   sec=rsec(j1)
   evla=revla(j1)
   evlo=revlo(j1)
   depth=rdepth(j1)
   mag=rmag(j1)
   ID=iID(j1)
   call date(year,month,day,days)
   call ndays2(re_year,re_days,year,days,n)
   O=((n*24.0d0+hour)*60.0d0+min)*60.0d0+sec*1.0d0
   !O=((((year-re_year)*365.0d0+days-re_days)*24.0d0+hour)*60.0d0+   &
   !      min)*60.0d0+sec*1.0d0
   stla=rhdr(32)
   stlo=rhdr(33)
   call GetDistAz(evla,evlo,stla,stlo,dist,az)
   !F=dist*1.0d0/2+O         !����ʱ�䷶Χ:����ʱ��-���о�/2
   F=dist*1.0d0+O         !����ʱ�䷶Χ:����ʱ��-���о�
   if((F-O)<30.0d0) F=30.d0+O  !������Сʱ�䷶ΧΪ����ʱ�̺�30s
   if(O<bm.or.O>em) cycle   !�¼�����SAC�ļ�ʱ�䷶Χ��
   neve=neve+1
   p=0
   do j=1,npha(j1)
    tempc1=csta(j1,j)
    call upper(tempc1)
    if(tempc1==sta(i)) then
     p=1
     exit
    endif
   enddo
   if(p==0) cycle  !���¼�û�б����̨վ��¼��
   nn=nint((F-O)/DT)+1
   allocate(data(nn))
   data=0.0e0
   p=0
   do j=1,npos
    n=-1
    if(B(j)<=O.and.E(j)>=O.and.E(j)<=F) then
     head=1
     tail=nint((E(j)-O)/DT)+1
     ahead=nint((O-B(j))/DT)+1
     atail=NPTS(j)
     n=-2
    elseif(B(j)>=O.and.E(j)<=F) then
     head=nint((B(j)-O)/DT)+1
     tail=nint((E(j)-O)/DT)+1
     ahead=1
     atail=NPTS(j)
     n=-2
    elseif(B(j)>=O.and.B(j)<=F.and.E(j)>=F) then
     head=nint((B(j)-O)/DT)+1
     tail=nn
     ahead=1
     atail=nint((F-B(j))/DT)+1
     n=-2
    elseif(B(j)<=O.and.E(j)>=F) then
     head=1
     tail=nn
     ahead=nint((O-B(j))/DT)+1
     atail=nint((F-B(j))/DT)+1
     n=-2
    endif
    if(n==-2) then
     p=1
     !n=pos(j)
     !allocate(adata(NPTS(j)))
     adata=0.0e0
     call brsac(100,NPTS(j),sacname(pos(j)),adata,nerr)
     data(head:tail)=adata(ahead:atail)
     !deallocate(adata)
    endif
   enddo
   if(p/=1) then
    deallocate(data)
    cycle    !���ݼ�¼Ϊ��,����
   endif
   head=1    !�޳��������˵�0ֵ
   tail=nn
   do j=1,nn
    if(data(j)/=0.0e0) exit
   enddo
   head=j
   do j=nn,1,-1
    if(data(j)/=0.0e0) exit
   enddo
   tail=j
   m=abs(tail-head)+1
   allocate(bdata(m))
   bdata(1:m)=data(head:tail)
   nsec=floor(sec)
   nmsec=nint((sec-nsec)*1000)
   !��ʵ��SAC�ļ�ͷ����,-12345.0��ʾδ����;����:-12345;�ַ���:��-12345..��
   ihdr(1)=year
   ihdr(2)=days
   ihdr(3)=hour
   ihdr(4)=min
   ihdr(5)=nsec
   ihdr(6)=nmsec  !������ʱ����Ϊ�ο�ʱ��
   ihdr(10)=m     !NPTS���ݳ���
   ihdr(13)=ID    !NXSIZE,��Ϊ�¼�ID
   ihdr(18)=11    !��Ч��׼ʱ��IZTYPE,9:BEGIN TIME,11:EVENT ORIGIN TIME
   ihdr(39)=1     !���DIST��AZ��BAZ��GCARC������̨վ���¼���������,���ֵ(LCALDA)ΪTRUE:1,����ΪFALSE:0
   rhdr(5)=-12345.0e0 !�ǵȼ������ODELTA
   !rhdr(6)=0.0e0  !��һ��������ʱ��B
   rhdr(6)=(head-1)*DT  !��һ��������ʱ��B
   rhdr(7)=rhdr(6)+(ihdr(10)-1)*DT  !���һ��������ʱ��E
   rhdr(8)=0.0e0  !����ʱ��O
   rhdr(9)=-12345.0e0  !P����ʱA,-12345.0��ʾ��Ч
   rhdr(11)=-12345.0e0 !S����ʱT0,-12345.0��ʾ��Ч
   rhdr(36)=evla  !�¼�γ��
   rhdr(37)=evlo  !�¼�����
   rhdr(39)=depth !��Դ���EVDP
   rhdr(40)=mag   !��
   rhdr(51)=dist !���о�DIST
   rhdr(52)=az !�¼���̨վ�ķ�λ��(��)AZ
   if(rhdr(52)<180.0e0) then
    rhdr(53)=rhdr(52)+180.0e0
   else
    rhdr(53)=rhdr(52)-180.0e0 !̨վ���¼��ķ�λ��(��)BAZ
   endif
   rhdr(54)=0.0e0 !̨վ���¼��Ĵ�Բ������(��)GCARC
   chdr(1)='        '
   chdr(1)=trim(sta(i)) !̨վ��
   print*, chdr(1),ihdr(13)
   do j=1,npha(j1)
    tempc1=csta(j1,j)
    tt=rtt(j1,j)
    weight=rweight(j1,j)
    phase=cphase(j1,j)
    call upper(tempc1)
    if(tempc1/=sta(i)) cycle
    if(phase=='P') then
     rhdr(9)=tt
     chdr(6)='P'
    else
     rhdr(11)=tt
     chdr(7)='S'
    endif
   enddo
   if(rhdr(9)==-12345.0e0) then
    chdr(6)='calP'
    p=1
   else
    if(rhdr(9)<rhdr(6)+5*DT.or.rhdr(9)>rhdr(7)-5*DT) then
     p=1
    else
     P=0
     n=nint((rhdr(9)-rhdr(6))/DT)+1
     do j=n-2,n+2
      if(bdata(j)==0.0e0) then
       p=1
       exit
      endif
     enddo
    endif
   endif
   if(rhdr(11)==-12345.0e0) then
    chdr(7)='calS'
    k=1
   else
    if(rhdr(11)<rhdr(6)+5*DT.or.rhdr(11)>rhdr(7)-5*DT) then
     k=1
    else
     k=0
     n=nint((rhdr(11)-rhdr(6))/DT)+1
     do j=n-2,n+2
      if(bdata(j)==0.0e0) then
       k=1
       exit
      endif
     enddo
    endif
   endif
   !if(rhdr(9)/=-12345.0e0.or.rhdr(11)/=-12345.0e0) then
   if(p==0.or.k==0) then
    write(tname(1:4),'(i4)') year
    if(month.lt.10) then
     tname(5:5)='0'
     write(tname(6:6),'(i1)') month
    else
     write(tname(5:6),'(i2)') month
    endif
    if(day.lt.10) then
     tname(7:7)='0'
     write(tname(8:8),'(i1)') day
    else
     write(tname(7:8),'(i2)') day
    endif
    if(hour.lt.10) then
     tname(9:9)='0'
     write(tname(10:10),'(i1)') hour
    else
     write(tname(9:10),'(i2)') hour
    endif
    if(min.lt.10) then
     tname(11:11)='0'
     write(tname(12:12),'(i1)') min
    else
     write(tname(11:12),'(i2)') min
    endif
    if(nsec.lt.10) then
     tname(13:13)='0'
     write(tname(14:14),'(i1)') nsec
    else
     write(tname(13:14),'(i2)') nsec
    endif
    wname=trim(sta(i))//'.'//tname(1:14)//'.'//com(i)//'.SAC'
    print*, trim(wname)
    !call wsac(data,nn,trim(wname))
    call wsac(bdata,m,trim(wname))
    write(10,*) trim(wname),dist,depth,mag,ID
   endif
   deallocate(data,bdata)
  enddo
  deallocate(B,E,NPTS,adata)
  !write(10,*) 'The station ',trim(sta(i))//'.'//com(i),' should record',neve,'events according to the earthquake catalog'
  !write(10,*) '-----'
  call system('mkdir done')
  name=sacname(pos(1))
  call dot(name,1,m)
  call dot(name,3,n)
  !tempc1=name(m+1:n-1)
  tempc2=name(n+3:n+3)
  command='mv 2*.'//name(m+1:n-1)//'.??'//tempc2//'.*  done/'
  print*, trim(command)
  call system(trim(command))
 enddo


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


subroutine isnul(string)
 !�ַ�����nul��Ϊ�ո�
 implicit none
 integer*4::i,strlen
 character*(*)::string
 strlen=len_trim(string)
 do i=1,strlen
  if(ichar(string(i:i))==0) then
   string(i:i)=' '  !ichar('NUL')=0  ichar(' ')=32
  endif
 enddo
 return
end subroutine


subroutine lower(string)
 !�ַ�����д��Сд
 implicit none
 integer*4::i,strlen
 character*(*)::string
 strlen=len_trim(string)
 do i=1,strlen
  if(string(i:i)>='A'.and.string(i:i)<='Z') then
   string(i:i)=char(ichar(string(i:i))+ichar('a')-ichar('A'))
  endif
 enddo
 return
end subroutine


subroutine upper(string)
 !�ַ���Сд���д
 implicit none
 integer*4::i,strlen
 character*(*)::string
 strlen=len_trim(string)
 do i=1,strlen
  if(string(i:i)>='a'.and.string(i:i)<='z') then
   string(i:i)=char(ichar(string(i:i))-ichar('a')+ichar('A'))
  endif
 enddo
 return
end subroutine


subroutine ndays2(yearS,daysS,yearE,daysE,nday)
!��������yearS-daysS��yearE-daysE�������
 implicit none
 integer*4::yearS,daysS,yearE,daysE,nday
 integer*4::nyear,nRN,day1,day2,day3
 integer*4::i,bet(3),n(3)!,mon(12)
 !data mon /31,28,31,30,31,30,31,31,30,31,30,31/
 !�ж��Ƿ�����:�ܱ�4���������ܱ�100�����Լ��ܱ�400���������Ϊ���ꡣ
 day1=daysS
 day3=daysE
 nyear=yearE-1-yearS
 bet(1)=4
 bet(2)=100
 bet(3)=400
 do i=1,3
  if(mod(nyear,bet(i))==0) then
   n(i)=nyear/bet(i)
  else
   if(mod(nyear,bet(i))>=mod(yearE-1,bet(i))) then
    n(i)=nyear/bet(i)+1
   else
    n(i)=nyear/bet(i)
   endif
  endif
 enddo
 nRN=n(1)-n(2)+n(3)
 day2=(nyear+1)*365+nRN
 nday=day2-day1+day3
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
 ihdr(10)=maxpts
 return
end subroutine


subroutine date(year,month,day,days)
!������ת��Ϊһ���еĵڼ���;��SAC�е�NZJDAY
 implicit none
 integer*4::i,year,month,day,days
 integer*4::mon(12)
 data mon /31,28,31,30,31,30,31,31,30,31,30,31/
 !�ж��Ƿ�����:�ܱ�4���������ܱ�100�����Լ��ܱ�400���������Ϊ���ꡣ
 if(mod(year,4)==0.and.mod(year,100)/=0) then
  mon(2)=29
 elseif(mod(year,400)==0) then
  mon(2)=29
 endif
 days=day
 do i=1,month-1
  days=days+mon(i)
 enddo
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

 
subroutine wsac(data,n,wname)
 implicit none
 integer*4::i,n,len
 real*4::min,max,sum
 real*4::data(n)
 character*(*)::wname
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)
 ihdr(10)=n
 min=1.0e+38
 max=-1.0e+38
 sum=0.0e0
 do i=1,ihdr(10)
	if(min.gt.data(i)) min=data(i)
	if(max.lt.data(i)) max=data(i)
  sum=sum+data(i)
 enddo
 rhdr(2)=min
 rhdr(3)=max
 rhdr(57)=sum/ihdr(10)
!  NZYEAR=ihdr(1) NZJDAY=ihdr(2) NZHOUR=ihdr(3) NZMIN=ihdr(4)
!  NZSEC=ihdr(5)  NZMSEC=ihdr(6)
!  ihdr(1)=year
!  ihdr(2)=day
 !print*, wname
 len=len_trim(wname)
 call bwsac(20,ihdr(10),wname(1:len),data)
 return
end subroutine

 
subroutine bwsac(IWU,LN,name,data)
!-----
!  This routine writes out a waveform data in SAC binary format.
!
!  Written by Hafidh A. A. Ghalib, 1988.
!
!  Modified by Wang, 2012
!-----
 implicit none
 integer*4::i,k,j,l,IWU,LN,nerr,nrec
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)
 character*(*)::name
 real*4::data(LN)
!-----
!  remove the original file so that the output length is
!  never greater than desired. Else the dregs of the 
!  first will remain
!-----
 open(IWU,file=name,form='unformatted',access='sequential',status='unknown')
 rewind(IWU)
 close(IWU,status='delete')
 ihdr(10)=LN
!  The actual number of waveform data points is stored in integer
!  header 10. The file recored length is 158*4=632.
 nrec=632+4*ihdr(10)
 open(IWU,file=name,form='unformatted',access='direct',recl=nrec,status='unknown')
 write(IWU,rec=1) (rhdr(i),i=1,70),     &
                  (ihdr(k),k=1,40),     &
                  (chdr(j),j=1,24),     & 
                  (data(l),l=1,LN)
 close(IWU)
 return
end subroutine
