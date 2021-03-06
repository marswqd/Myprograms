﻿!功能: 合并ph2dt型地震目录
!作者: 王清东  时间: 2013/11/7 15:10:54
program hblog
 implicit none
 integer*4::year1,month1,day1,hour1,minute1,year2,month2,day2,hour2,minute2,id1,id2,i,s16p,s30p,s16s,s30s
 real*4::sec1,sec2,dt,evla1,evlo1,evla2,evlo2,yl(6,2),dist,az,tt,weight
 real*4::dep1,mag1,dep2,mag2
 character::flag*1,card1*150,card2*150,sta*3,phase*1,temp*2,stayl(6)*3


 open(10,file='ylynsc.txt')
 open(11,file='ylyl.txt')
 open(12,file='a.txt')
 open(13,file='all.txt')
 open(14,file='stationyl.txt')
 do i=1,6
  read(14,*) stayl(i),yl(i,1),yl(i,2)
 enddo 
 do
  read(10,'(a150)',end=100) card1
  if(card1(1:1)=='#'.or.card1(2:2)=='#') then
   s16p=0
   s30p=0
   s16s=0
   s30s=0
   read(card1,*) flag,year1,month1,day1,hour1,minute1,sec1,evla1,evlo1,dep1,mag1,dt,dt,dt,id1
   write(13,'(a)') trim(card1)
   do
    read(10,'(a150)',end=100) card1
    if(card1(1:1)=='#'.or.card1(2:2)=='#') then
     backspace(10)
     exit
    endif
    read(card1,*) sta,tt,weight,phase
    if(sta=='YIL'.and.phase=='P') s16p=1
    if(sta=='YIL'.and.phase=='S') s16s=1
    if(sta=='QIJ'.and.phase=='P') s30p=1
    if(sta=='QIJ'.and.phase=='S') s30s=1
    write(13,'(a)') trim(card1)
   enddo
   do
    read(11,'(a150)',end=101) card2
    if(card2(1:1)=='#'.or.card2(2:2)=='#') then
     !print*, card2
     read(card2,*) flag,year2,month2,day2,hour2,minute2,sec2,evla2,evlo2,dep2,mag2,dt,dt,dt,id2
     !print*, flag,year2,month2,day2,hour2,minute2,sec2
     if(year1==year2.and.month1==month2) then
      dt=9999.0e0
      dt=(((day2-day1)*24+(hour2+8-hour1))*60+(minute2-minute1))*60.0e0+sec2-sec1
      !print*, dt
      if(abs(dt)<=2.5e0) then
       write(*,*) trim(card1)
       !write(12,'(a150)') card1
       !write(12,'(a150)') card2
       write(12,*) year1,month1,day1,hour1,minute1,sec1,evla1,evlo1,dep1,mag1,id1
       write(12,*) year2,month2,day2,hour2+8,minute2,sec2,evla2,evlo2,dep2,mag2,id2
       write(12,*) 'dt=',dt
       do
        read(11,'(a150)',end=101) card2
        if(card2(1:1)=='#'.or.card2(2:2)=='#') then
         backspace(11)
         exit
        endif
        read(card2,*) sta,tt,weight,phase,temp,id2
        dist=0.0e0
        az=0.0e0
        do i=1,6
         if(stayl(i)==sta) then
          call GetDistAz(evla1,evlo1,yl(i,1),yl(i,2),dist,az)
         exit
         endif
        enddo
        if(sta=='S16') then
         if(s16p==1.and.phase=='P') cycle
         if(s16s==1.and.phase=='S') cycle
         sta='YIL'
        endif
        if(sta=='S30') then
         if(s30p==1.and.phase=='P') cycle
         if(s30s==1.and.phase=='S') cycle
         sta='QIJ'
        endif
        if(phase=='P') then
         write(13,'(a3,2x,f7.3,4x,a,4x,f7.3,4x,f6.2,i9)') sta,tt+dt,'1.000   P',dist,az,id2
        else
         write(13,'(a3,2x,f7.3,4x,a,4x,f7.3,4x,f6.2,i9)') sta,tt+dt,'0.500   S',dist,az,id2
        endif
       enddo
      endif
     endif
    endif
   enddo
  endif
  101 rewind(11)
 enddo
 100 continue


end program


subroutine GetDistAz(lat1,long1,lat2,long2,dist,az)
!
!longitude:;latitude:
!,,,
!1,2,az12;
!1,2, az;
 implicit none
 real*4::lat1,long1,lat2,long2,dist,az
 real*4::radlat1,radlong1,radlat2,radlong2,a,b,radlat
 real*4,parameter::EARTH_RADIUS=6371.004e0  !
 !real*4,parameter::EARTH_RADIUS=6378.137e0  !
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

 
