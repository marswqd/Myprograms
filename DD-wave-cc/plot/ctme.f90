!功能: 处理由双差定位软件的预处理程序ph2dt生成的地震目录走时差文件dt.ct,
!      读入台站和地震信息文件,计算震中距和方位角,生成新文件用于GMT绘图
!      ctdt=tt1-tt2
!作者: 王清东 时间: 2014/4/29 9:42:38
program ctme
 implicit none
 integer*4::i,j,k,n,ID,ID1,ID2,nline,nph,np,ns,npho,npo,nso,ne,neo
 integer*4::year,month,day,hour,minute
 real*4::T1,T2,tt,tt1,tt2,dt,vp,vs,maxttp,maxtts,maxtt,weight
 real*4::sec,dist,dist1,dist2,az,az1,az2,d12,e,mag,evdp,elat,elon
 character*1::flag,phase,pha
 character*5::sta,st
 character*60::filetd,filect


 call CPU_TIME(T1)
 !open(9,file='ccme.log')


!初始互相关走时差文件
 call getarg(1,filetd) !地震目录走时差文件dt.ct
 call exist(filetd)
 open(10,file=filetd,status='old',action='read')
 call getarg(2,filect) !地震目录文件
 call exist(filect)
 open(11,file=filect,status='old',action='read')
 open(12,file='ctme.txt') !生成的新文件,用于GMT绘图


 print*, 'Step 1'
 nph=0
 np=0
 ns=0
 npho=0
 npo=0
 nso=0
 ne=0
 neo=0

 rewind(10)
 do
  read(10,*,end=20) flag
  if(flag=='#') then  !找到事件标示
   backspace(10)
   read(10,*) flag,ID1,ID2,d12
   nline=0
   do                 !找到事件对应的震相数
    read(10,*,end=30) flag
    if(flag=='#') exit
    nline=nline+1
   enddo
   30 continue
   do i=1,nline+1
    backspace(10)
   enddo
   if(nline==0) cycle     !无震相记录
   print*, '#',ID1,ID2,d12
   do i=1,nline
    read(10,*) sta,tt1,tt2,weight,phase
    dt=tt1-tt2
    k=0
    rewind(11)
    do
     read(11,*,end=22) flag
     if(flag=='#') then  !找到事件标示
      backspace(11)
      read(11,*) flag,year,month,day,hour,minute,sec,elat,elon,evdp,mag,e,e,e,ID
      if(ID==ID1.or.ID==ID2) then
       n=0
       do                 !找到事件对应的震相数
        read(11,*,end=24) flag
        if(flag=='#') exit
        n=n+1
       enddo
       24 continue
       do j=1,n+1
        backspace(11)
       enddo
       if(n==0) cycle     !无震相记录
       k=k+1
       do j=1,n
        read(11,*) st,tt,weight,pha,dist,az
        if(sta==st.and.ID==ID1) then
         dist1=dist
         az1=az
         exit
        elseif(sta==st.and.ID==ID2) then
         dist2=dist
         az2=az
         exit
        endif
       enddo
       write(12,*) ID1,ID2,d12,phase,sta,dt,tt1,tt2,dist1,dist2,az1,az2
      endif
     endif
     if(k==2) goto 22
    enddo
    22 continue
   enddo
  endif
 enddo
 20 continue
 close(10)
 close(11)


 print*, 'Original  event-pairs  phases  P-phase S-phase'
 print*, neo,npho,npo,nso
 print*, 'Selected  event-pairs  phases  P-phase S-phase'
 print*, ne,nph,np,ns
 write(9,*)
 write(9,*) 'Original  event-pairs  phases  P-phase S-phase'
 write(9,*) neo,npho,npo,nso
 write(9,*) 'Selected  event-pairs  phases  P-phase S-phase'
 write(9,*) ne,nph,np,ns


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
