program getda3
 implicit none

 integer*4::i,j,k,IRU,nerr,num
 real*4::dist12,az12,dist13,az13,dist23,az23,d1,d2,d3,temp
 real*4,allocatable::lat(:),long(:)
 character*4,allocatable::KSTNM(:)
 character*60,allocatable::sacname(:)

 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)

! 读取控制文件信息
 open(11,file='getfile',status='old')
 read(11,*) num
 allocate(sacname(num),lat(num),long(num),KSTNM(num))
 do i=1,num
  read(11,*) sacname(i)
  call filesta(sacname(i),nerr)
  if(nerr.eq.1) then
   print*, 'The file ',trim(sacname(i)),' dosen`t exist'
   goto 99
  endif
  sacname(i)=adjustl(sacname(i))
  print*, trim(sacname(i))
 enddo
 close(11)

 IRU=100
!  DT=rhdr(1)     B=rhdr(6)      O=rhdr(8)      DIST=rhdr(51)
!  STLA=rhdr(32)  STLO=rhdr(33)  STEL=rhdr(34)  STDP=rhdr(35)
!  EVLA=rhdr(36)  EVLO=rhdr(37)  EVEL=rhdr(38)  EVDP=rhdr(39)
!  NZYEAR=ihdr(1) NZJDAY=ihdr(2) NZHOUR=ihdr(3) NZMIN=ihdr(4)
!  NZSEC=ihdr(5)  NZMSEC=ihdr(6) NPTS=ihdr(10)  KSTNM=chdr(1)
 do i=1,num
  call brsach(IRU,sacname(i),nerr)           !读SAC文件头，提取必要信息
  lat(i)=rhdr(32)
  long(i)=rhdr(33)
  KSTNM(i)=chdr(1)
 enddo
 open(12,file='distaz3.txt')
 open(13,file='name3.txt')
 write(12,*) '  台站对  ','      台站间距   ','         方位角'
 do i=1,num-2
  do j=i+1,num-1
   do k=j+1,num
    call GetDistAz(lat(i),long(i),lat(j),long(j),dist12,az12)
    call GetDistAz(lat(i),long(i),lat(k),long(k),dist13,az13)
    call GetDistAz(lat(j),long(j),lat(k),long(k),dist23,az23)
    d1=dist12
    d2=dist13
    d3=dist23
    if(d1.gt.d2) then
     temp=d1
     d1=d2
     d2=temp
    endif
    if(d1.gt.d3) then
     temp=d1
     d1=d3
     d3=temp
    endif
    if(d2.gt.d3) then
     temp=d2
     d2=d3
     d3=temp
    endif
    if(d1.ge.200.0e0.and.d2.ge.200.0e0) then
     temp=abs((d1+d2)/d3-1)
     if(temp.le.0.005e0) then
      write(12,*) trim(KSTNM(i)),trim(KSTNM(j)),dist12,az12,        &
                  trim(KSTNM(i)),trim(KSTNM(k)),dist13,az13,        &
                  trim(KSTNM(j)),trim(KSTNM(k)),dist23,az23
      write(13,'(3a8)') trim(KSTNM(i)),trim(KSTNM(j)),trim(KSTNM(k))
     endif
    endif
   enddo
  enddo
 enddo

 99 continue
 deallocate(sacname,lat,long,KSTNM)
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


subroutine GetDistAz(lat1,long1,lat2,long2,dist,az)
!由两点经纬度计算大圆劣弧长和方位角
! longitude:经度;  latitude:纬度
 real*4::lat1,long1,lat2,long2,dist,az
 real*4::radlat1,radlong1,radlat2,radlong2,a,b,radlat
 real*4,parameter::EARTH_RADIUS=6378.137e0
 real*4,parameter::pi=3.1415926535898e0
 radlat1=lat1*pi/180.0e0
 radlong1=long1*pi/180.0e0
 radlat2=lat2*pi/180.0e0
 radlong2=long2*pi/180.0e0
 a=radLat1-radLat2
 b=radlong1-radlong2
 radlat=(radlat1+radlat2)/2
 dist=2*asin(sqrt(sin(a/2)*sin(a/2)+cos(radlat1)*cos(radlat2)*sin(b/2)*sin(b/2)))*EARTH_RADIUS
 if(lat1.eq.lat2) then
  az=90.0e0
 else
  az=atan((long1-long2)/(lat1-lat2)*cos(radlat))*180/pi
 endif
 if(lat1.gt.lat2) then
  az=az+180
 endif
 if(az.lt.0.0e0) then
  az=360+az
 endif
 return
end subroutine


subroutine GetAngle(xa,ya,xb,yb,xc,yc,BAC)
!由三点经纬度计算两条弧线在球面上的顺时针交角
! xa, ya （两条弧线相交点的经度和纬度）
! xb, yb （起始弧线的起始点的经度和纬度）
! xc, yc （终止弧线的终点的经度和纬度）
! BAC    （所求球面角）
 real*4::xa,ya,xb,yb,xc,yc,BAC,a,b,c,d
 real*4,parameter::pi=3.1415926535898e0
 xa=xa*pi/180
 ya=ya*pi/180
 xb=xb*pi/180
 yb=yb*pi/180
 xc=xc*pi/180
 yc=yc*pi/180
 a=acos(cos(yb)*cos(yc)*cos(xb-xc)+sin(yb)*sin(yc))!求圆心角（弧度表示）
 b=acos(cos(ya)*cos(yc)*cos(xa-xc)+sin(ya)*sin(yc))!求圆心角（弧度表示）
 c=acos(cos(ya)*cos(yb)*cos(xa-xb)+sin(ya)*sin(yb))!求圆心角（弧度表示）
 d=(cos(a)-cos(b)*cos(c))/(sin(b)*sin(c))
 BAC=acos(d)*180/pi!求弧线夹角（单位度）
 return
end subroutine





