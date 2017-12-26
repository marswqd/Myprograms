!功能：从地震目中挑选符合要求的数据，生成新的地震目录
!作者：王清东   时间：2016/5/6 11:24:20
program log2log
 implicit none
 integer*4::i,j,k,npha,ID,newID,stat
 integer*4::year,month,day,hour,minute,byr,eyr,bmn,emn,bdy,edy,bhr,ehr
 integer*4::btime(4),etime(4),nday,bt,et
 real*4::bspace(3),espace(3),amag(2),stla,stlo,depth,mag,rtmp,sec
 character::catalog*60,newlog*60,flag*1,line*100,pos*20,posn*20

 !参数设定
 catalog='2014all.txt'    !输入地震目录 ph2dt型
 newlog='log_jg.txt'    !输出地震目录 ph2dt型
 data btime/2014,9,7,0/   !时间范围，精确到小时
 data etime/2015,1,1,0/
 !data bspace/23.2,100.3,0/       !空间范围，纬度 经度 深度
 !data espace/23.6,100.7,1000/
 data bspace/0,0,0/       !空间范围，纬度 经度 深度
 data espace/90,180,1000/
 data amag/1.0,10.0/     !震级范围，最小震级 最大震级
 npha=4
 posn='云南景谷'

 open(10,file=catalog,status='old')
 open(11,file=newlog)
 newID=0
 do
  read(10,'(a100)',end=20) line
  read(line,*) flag
  if(flag=='#') then
   !print*,line
   read(line,*,iostat=stat) flag,year,month,day,hour,minute,sec,stla,    &
                            stlo,depth,mag,rtmp,rtmp,rtmp,ID,pos
   if(stat/=0) goto 30
   goto 31
   30 read(line,*) flag,year,month,day,hour,minute,sec,stla,    &
                   stlo,depth,mag,rtmp,rtmp,rtmp,ID
   pos='null'
   31 continue
   call ndays(btime(1),btime(2),btime(3),year,month,day,nday)
   !print*, pos,posn
   !print*,nday
   bt=nday*24+hour-btime(4)
   call ndays(year,month,day,etime(1),etime(2),etime(3),nday)
   et=nday*24+etime(4)-hour
   if(bt<0.or.et<0) cycle  !地震事件超出时间范围
   if(stla<bspace(1).or.stla>espace(1)) cycle  !地震事件超出空间范围
   if(stlo<bspace(2).or.stlo>espace(2)) cycle
   if(depth<bspace(3).or.depth>espace(3)) cycle
   if(mag<amag(1).or.mag>amag(2)) cycle        !超出震级范围
   if(pos/=posn) cycle !超出地域
   k=0
   do
    read(10,*,end=22) flag
    if(flag=='#') exit
    k=k+1
   enddo
   22 continue
   do j=1,k+1
    backspace(10)
   enddo
   !print*,k
   if(k<npha) cycle    !此事件震相记录少于要求
   newID=newID+1
   write(11,'(a)') trim(line)
   do j=1,k                !找到事件对应的震相数
    read(10,'(a100)') line
    write(11,'(a)') trim(line)
   enddo
  endif
 enddo
 20 continue
 print*, 'Events: ',newID
 close(10)
 close(11)


 99 continue
end program


subroutine ndays(yearS,monthS,dayS,yearE,monthE,dayE,nday)
!计算日期yearS-monthS-dayS至yearE-monthE-dayE间的天数
 implicit none
 integer*4::yearS,monthS,dayS,yearE,monthE,dayE,nday
 integer*4::nyear,nRN,day1,day2,day3
 integer*4::i,bet(3),n(3),mon(12)
 data mon /31,28,31,30,31,30,31,31,30,31,30,31/
 !判断是否闰年:能被4整除但不能被100整除以及能被400整除的年份为闰年。
 if(mod(yearS,4)==0.and.mod(yearS,100)/=0) then
  mon(2)=29
 elseif(mod(yearS,400)==0) then
  mon(2)=29
 endif
 day1=dayS
 do i=1,monthS-1
  day1=day1+mon(i)
 enddo
 mon(2)=28
 if(mod(yearE,4)==0.and.mod(yearE,100)/=0) then
  mon(2)=29
 elseif(mod(yearE,400)==0) then
  mon(2)=29
 endif
 day3=dayE
 do i=1,monthE-1
  day3=day3+mon(i)
 enddo
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

