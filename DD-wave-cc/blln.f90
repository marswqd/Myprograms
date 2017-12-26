!地震对走时差数据格式转化，由块类型 与 行类型 互相转换
!   #  id1   id2   d12
!   sta  tt(or tt1 tt2)  weight  phase
! <>
!   id1   id2   d12  sta  tt  phase  weight  tt1  tt2
!作者：王清东   日期：2016/6/3 9:23:36
program blln
 implicit none

 integer*4::i,j,id1,id2,nep,nline,np,ns,idx,ict,iflag,idd1(200),idd2(200),iv
 real*4::d12,tt,tt1,tt2,weight,cdt,odt,otc
 !real*4,allocatable::
 character::flag*1,phase*1,sta*7,card(200)*200,input*50,output*50


 ict=1  !0: 行ct to 块cc ; 1: 行ct to 块ct ;
 input='dt_add.ct'
 output='ct_add.txt'

 open(9,file='blln.log')
 open(10,file=trim(input),status='old',action='read')  !输入地震对走时数据
 open(11,file=trim(output))  !输出
 110 format(2(i10,1x),f7.3,1x,a,1x,f7.3,1x,f7.3,1x,a,1x,f7.3,1x,i1)

 iflag=1
 do i=1,100
  read(10,'(a200)',end=16) card(1)
  j=index(card(1),'#')
  if(j/=0) then  !找到字符#，输入数据为块类型，输出数据应为行类型
   iflag=0
   exit
  endif
 enddo
 16 continue

 if(iflag==0) then !块 to 行
  rewind(10)
  nep=0
  np=0
  ns=0
  do
   read(10,*,end=18) flag
   if(flag=='#') then  !找到事件标示
    nep=nep+1
    backspace(10)
    read(10,'(a200)') card(1)
    !PRINT*, card(1)
    !read(card(1),*,err=12) flag,id1,id2,otc,d12
    read(card(1),*,iostat=iv) flag,id1,id2,otc,d12
    if(iv<0) goto 12
    goto 13
    12 read(card(1),*) flag,id1,id2
    d12=-9
    13 continue
    nline=0
    do                 !找到事件对应的震相数
     read(10,*,end=20) flag
     if(flag=='#') exit
     nline=nline+1
    enddo
    20 continue
    do i=1,nline+1
     backspace(10)
    enddo
    if(nline==0) cycle     !无震相记录
    print*, '#',id1,id2,d12
    do i=1,nline
     read(10,'(a200)') card(1)
     !read(card(1),*,err=14) sta,tt1,tt2,weight,phase
     read(card(1),*,iostat=iv) sta,tt1,tt2,weight,phase
     if(iv<0) goto 14
     idx=4    !ct
     goto 15
     14 read(card,*) sta,tt1,weight,phase
     idx=2    !cc
     tt2=0
     15 continue
     tt=tt1-tt2
     if(phase=='P') then
      np=np+1
      if(idx<=2) then
       idx=1  !ccp
      else
       idx=3  !ctp
      endif
     else
      ns=ns+1
      if(idx<=2) then
       idx=2  !ccs
      else
       idx=4  !cts
      endif
     endif
     write(11,110) id1,id2,d12,sta,tt,weight,phase,tt1,idx
    enddo
   endif
  enddo
  18 continue
  close(10)
  close(11)
 endif


 if(iflag/=0) then !行 to 块
  rewind(10)
  nep=0
  np=0
  ns=0
  i=0
  otc=0.0e0
  do                 !id1,id2,d12,sta,pdt1,cc1,phase,cdt1,adist,aaz,daz
   i=i+1
   read(10,'(a200)',end=30) card(i)
   read(card(i),*) idd1(i),idd2(i)
   if(i==1) cycle
   if(idd1(i-1)/=idd1(i).or.idd2(i-1)/=idd2(i)) then
    nep=nep+1
    read(card(1),*) id1,id2,d12
    print*, '#',id1,id2,d12
    write(11,'(a,1x,i10,1x,i10,1x,f5.2,1x,f7.3)') '#',id1,id2,otc,d12
    do j=1,i-1
     read(card(j),*) id1,id2,d12,sta,tt,weight,phase,tt1,idx
     if(idx<=2) then   !cc
      write(11,'(a,1x,f7.3,1x,f5.2,1x,a,1x,f7.3,1x,i1)') sta,tt,weight,phase,tt1,idx
      if(idx==1) then
       np=np+1
      else
       ns=ns+1
      endif
     else  !ct
      if(ict==1) then
       tt2=tt1-tt
       write(11,'(a,1x,f7.3,1x,f7.3,1x,f5.2,1x,a,1x,i1)') sta,tt1,tt2,weight,phase,idx
      else
       write(11,'(a,1x,f7.3,1x,f5.2,1x,a,1x,f7.3,1x,i1)') sta,tt,weight,phase,tt1,idx
      endif
      if(idx==3) then
       np=np+1
      else
       ns=ns+1
      endif
     endif
    enddo
    i=0
    backspace(10)
   endif
  enddo
  30 continue
  nep=nep+1
  read(card(1),*) id1,id2,d12
  print*, '#',id1,id2,d12
  write(11,'(a,1x,i10,1x,i10,1x,f5.2,1x,f7.3)') '#',id1,id2,otc,d12
  do j=1,i-1
   read(card(j),*) id1,id2,d12,sta,tt,weight,phase,tt1,idx
   if(idx<=2) then   !cc
    write(11,'(a,1x,f7.3,1x,f5.2,1x,a,1x,f7.3,1x,i1)') sta,tt,weight,phase,tt1,idx
    if(idx==1) then
     np=np+1
    else
     ns=ns+1
    endif
   else  !ct
    if(ict==1) then
     tt2=tt1-tt
     write(11,'(a,1x,f7.3,1x,f7.3,1x,f5.2,1x,a,1x,i1)') sta,tt1,tt2,weight,phase,idx
    else
     write(11,'(a,1x,f7.3,1x,f5.2,1x,a,1x,f7.3,1x,i1)') sta,tt,weight,phase,tt1,idx
    endif
    if(idx==3) then
     np=np+1
    else
     ns=ns+1
    endif
   endif
  enddo
  close(10)
  close(11)
 endif



 print*, 'input: ',trim(input),'  output: ',trim(output)
 print*, 'event-pairs    Phase    P-phase    S-phase   phase/pair'
 print*, nep,np+ns,np,ns,(np+ns)*1.0e0/nep
 write(9,*) 'input: ',trim(input),'  output: ',trim(output)
 write(9,*) 'event-pairs    Phase    P-phase    S-phase   phase/pair'
 write(9,*) nep,np+ns,np,ns,(np+ns)*1.0e0/nep


 99 continue
end program

