!按发震时刻挑选相应的evt文件

program getevt
 implicit none

 integer*4::i,j,k,m,n,re_year,re_days,neve,nevt,year,mon,day,days,hour,min,yr,mn,dy,dys,hr,mi
 real*4::t0,t1,t2,t
 character::event*60,pwd*100,command*200
 character*60,allocatable::evtname(:),oldname(:)

 open(10,file='getevt.log')
!参考时间-时间基准
 re_year=2012
 re_days=1

!读取控制文件信息
 open(11,file='getevt.in',status='old')
 read(11,'(a60)') event                       !地震目录
 read(11,'(a100)') pwd                     !复制evt文件到evtfile文件夹
 read(11,*) nevt
 allocate(evtname(nevt),oldname(nevt))
 do i=1,nevt
  read(11,*) evtname(i)
  call exist(evtname(i))
  evtname(i)=adjustl(evtname(i))
  print*, trim(evtname(i))
 enddo
 close(11)
 event=trim(adjustl(event))
 pwd=trim(adjustl(pwd))
 print*
 print*

 open(12,file=event,status='old',action='read')
 call filelen(12,neve)
 m=1
 do i=1,neve
  read(12,*) year,mon,day,hour,min
  call date(year,mon,day,days)
  t0=(((year-re_year)*365.0e0+days-re_days)*24.0e0+hour)*60.0e0+min !单位为分钟
  t1=t0-60.0e0
  t2=t0+60.0e0
  print*, 'event O-time:',year,mon,day,hour,min
  write(10,*) 'event O-time:',year,mon,day,hour,min
  do j=1,nevt
   read(evtname(j)(1:4),"(i4)") yr
   read(evtname(j)(5:6),"(i2)") mn
   read(evtname(j)(7:8),"(i2)") dy
   read(evtname(j)(9:10),"(i2)") hr
   read(evtname(j)(11:12),"(i2)") mi
   call date(yr,mn,dy,dys)
   t=(((yr-re_year)*365.0e0+dys-re_days)*24.0e0+hr)*60.0e0+mi !单位为分钟
   n=0
   if(t>=t1.and.t<=t2) then
    if(i==1) then
     oldname(m)=evtname(j)
     m=m+1
    else
     do k=1,m
      if(oldname(k)==evtname(j)) then
       n=1
       cycle
      endif
     enddo
    endif
    if(n==1) cycle
    oldname(m)=evtname(j)
    m=m+1
    print*, trim(evtname(j))
    write(10,*) trim(evtname(j))
    command='cp '//trim(evtname(j))//' '//pwd
    !print*, pwd
    !print*, command
    call system(command,n)
   endif
  enddo
  print*
  write(10,*)
 enddo


end program


subroutine filelen(fileid,n)
!得到文件行数
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


subroutine indexx(n,arrin,indx)
!从小到大排序,只记录位置信息indx(n),不交换数组arrin
!从小到大显示数组arrin:
!do i=1,n
! write(*,*) arrin(indx(i))
!enddo
 implicit none
 integer*4::i,j,l,ir,n,indxt,indx(n)
 real*4::q,arrin(n)
 if (n.lt.1) then
  return
 elseif (n.eq.1) then
  indx(1) = 1
  return
 endif
 do j=1,n
  indx(j)=j
 enddo
 l=n/2+1
 ir=n
 10 continue
 if(l.gt.1)then
  l=l-1
  indxt=indx(l)
  q=arrin(indxt)
 else
  indxt=indx(ir)
  q=arrin(indxt)
  indx(ir)=indx(1)
  ir=ir-1
  if(ir.eq.1)then
   indx(1)=indxt
   return
  endif
 endif
 i=l
 j=l+l
 20 if(j.le.ir)then
     if(j.lt.ir)then
      if(arrin(indx(j)).lt.arrin(indx(j+1)))j=j+1
     endif
     if(q.lt.arrin(indx(j)))then
      indx(i)=indx(j)
      i=j
      j=j+j
     else
      j=ir+1
     endif
     goto 20
    endif
    indx(i)=indxt
    goto 10
end subroutine


subroutine date(year,month,day,days)
!将日期转化为一年中的第几天;即SAC中的NZJDAY
 implicit none
 integer*4::i,year,month,day,days
 integer*4::mon(12)
 data mon /31,28,31,30,31,30,31,31,30,31,30,31/
 if(mod(year,4).eq.0) mon(2)=29  !判断是否为闰年
 days=day
 do i=1,month-1
  days=days+mon(i)
 enddo
 return
end subroutine


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



