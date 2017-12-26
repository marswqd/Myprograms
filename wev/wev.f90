!������Ĺ�����:�������¼��ľ�γ��,���о�,����ʱ�̵�д��sac�ļ�
!����: ���嶫  ʱ��: 2012-11-12 16:47:12
program wev
 implicit none
 
 integer*4::i,j,nerr,num,NPTS
 integer*4::year,month,day,hour,min,nsec,nmsec,days
 real*4::sec,evla,evlo,depth,mag,t
 real*4,allocatable::ttp(:),tts(:)
 character*60::wname
 character*60,allocatable::sacname(:)
 
 real*4,allocatable::data(:)
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)


!��ȡ�����ļ���Ϣ 
 open(11,file='wev.in',status='old')
 read(11,*) year,month,day,hour,min,sec,evla,evlo,depth,mag !����ʱ��,γ��latitude,����longitude,���,��
 read(11,*) num                                       !num:�����sac�ļ���
 allocate(sacname(num),ttp(num),tts(num))                               !sacname:sac�ļ���
 do i=1,num
  read(11,*) sacname(i),ttp(i),tts(i)
  call filesta(sacname(i),nerr)
  if(nerr.eq.1) then
   print*, 'The file ',trim(sacname(i)),' dosen`t exist'
   goto 99
  endif
  sacname(i)=adjustl(sacname(i))
  !print*, trim(sacname(i))
 enddo
 close(11)
 
 call date(year,month,day,days)
 do i=1,num
  call brsach(100,sacname(i),nerr)
! DT=rhdr(1)     B=rhdr(6)      O=rhdr(8)      DIST=rhdr(51)
! A=rhdr(9)      T0=rhdr(11)    T1=rhdr(12)    T2=rhdr(13)
! STLA=rhdr(32)  STLO=rhdr(33)  STEL=rhdr(34)  STDP=rhdr(35)
! EVLA=rhdr(36)  EVLO=rhdr(37)  EVEL=rhdr(38)  EVDP=rhdr(39)
! NZYEAR=ihdr(1) NZJDAY=ihdr(2) NZHOUR=ihdr(3) NZMIN=ihdr(4)
! NZSEC=ihdr(5)  NZMSEC=ihdr(6) NPTS=ihdr(10)  KSTNM=chdr(1)
  !print*, 'DT=',rhdr(1)
  !DT=rhdr(1)*1.0d0
  NPTS=ihdr(10)
  allocate(data(NPTS))
  call brsac(100,NPTS,sacname(i),data,nerr)
  t=((((year-ihdr(1))*365.0d0+days-ihdr(2))*24.0d0+hour-8-ihdr(3))*60.0d0+   &
        min-ihdr(4))*60.0d0+(sec-ihdr(5))*1.0d0+ihdr(6)*0.001d0
  !t:����ʱ������ڲο�ʱ���ʱ����,��λs;
  !hour-8:Evt�ļ��е�ʱ��(��8��)=Sac�ļ��еĲο�ʱ��(GMT)+8��Сʱ
  if(t.lt.0.0e0) then
   print*, 'The file ',trim(sacname(i)),' dose not include the Orige time, please check!'
   !cycle
  endif
  !��ʵ��SAC�ļ�ͷ����,-12345.0��ʾδ����;����:-12345;�ַ���:��-12345..��
  nsec=floor(sec)
  nmsec=anint((sec-nsec)*1000)
  ihdr(1)=year
  ihdr(2)=days
  ihdr(3)=hour
  ihdr(4)=min
  ihdr(5)=nsec
  ihdr(6)=nmsec  !���ο�ʱ����Ϊ����ʱ��
  ihdr(18)=11    !��Ч��׼ʱ��IZTYPE,8:BEGIN TIME,11:EVENT ORIGIN TIME
  ihdr(39)=1     !���DIST��AZ��BAZ��GCARC������̨վ���¼���������,���ֵΪTRUE:1,����ΪFALSE:0
  rhdr(5)=-12345.0
  rhdr(6)=rhdr(6)-t !��һ��������ʱ��B
  rhdr(8)=0.0e0  !����ʱ��O
  rhdr(9)=ttp(i) !P����ʱA,-12345.0��ʾ��Ч
  rhdr(11)=tts(i)!S����ʱT0,-12345.0��ʾ��Ч
  rhdr(36)=evla  !�¼�γ��
  rhdr(37)=evlo  !�¼�����
  rhdr(39)=depth !��Դ���EVDP
  rhdr(40)=mag   !��
  rhdr(51)=0.0e0 !���о�DIST
  rhdr(52)=0.0e0 !�¼���̨վ�ķ�λ��(��)AZ
  rhdr(53)=0.0e0 !̨վ���¼��ķ�λ��(��)BAZ
  rhdr(54)=0.0e0 !̨վ���¼��Ĵ�Բ������(��)GCARC
  chdr(6)='P'    !ʱ���־KA
  chdr(7)='S'    !ʱ���־KT0
  !call GetDistAz(rhdr(32),rhdr(33),rhdr(36),rhdr(37),dist,az)
  !rhdr(51)=dist  !���о�
  !rhdr(52)=az    !�¼���̨վ�ķ�λ��(��)
  !print*, 'IZTYPE', ihdr(18)

  wname=trim(sacname(i))//'.SAC'
  call bwsac(100,NPTS,wname,data)
 enddo


 99 continue
end program


subroutine filesta(name,nerr)
 implicit none
 integer*4::nerr
 character*(*)::name
 logical::alive
 nerr=0
 inquire(file=name,exist=alive)
 if(alive.eqv..false.) then
  !print*, 'The file ',trim(name),' dosen`t exist'
  nerr=1
 endif
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
 if(mod(year,4).eq.0) mon(2)=29  !�ж��Ƿ�Ϊ����
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
 dist=acos(sin(radlat1)*sin(radlat2)+cos(radlat1)*cos(radlat2)*cos(b))*EARTH_RADIUS
 !dist=2*asin(sqrt(sin(a/2)*sin(a/2)+cos(radlat1)*cos(radlat2)*sin(b/2)*sin(b/2)))*EARTH_RADIUS
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
