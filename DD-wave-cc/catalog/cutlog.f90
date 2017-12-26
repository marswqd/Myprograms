!功能：从地震目中挑选符合要求的数据，生成新的地震目录
!作者：王清东   时间：2015/8/10 20:16:54
program cutlog
 implicit none
 integer*4::i,j,k,m
 integer*4::year,month
 character::catalog*60,newlog*60,flag*1,line*100,pos*20

 !参数设定
 catalog='2014all.txt'    !输入地震目录 ph2dt型
 year=2014


 open(10,file=catalog,status='old')
 newlog=''
 write(newlog(1:4),'(i4)') year
 do i=1,12
  rewind(10)
  if(i<10) then
   newlog(5:5)='0'
   write(newlog(6:6),'(i1)') i
  else
   write(newlog(5:6),'(i2)') i
  endif
  newlog(7:10)='.txt'
  print*, trim(newlog)
  open(100,file=newlog(1:10))
  do
   m=0
   read(10,'(a100)',end=20) line
   read(line,*) flag
   if(flag=='#') then
    read(line,*) flag,year,month
    if(month==i) then
     m=1
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
    endif
   endif
   if(m==1) then
    write(100,'(a)') trim(line)
    do j=1,k                !找到事件对应的震相数
     read(10,'(a100)') line
     write(100,'(a)') trim(line)
    enddo
   endif
  enddo
  20 continue
  close(100)
 enddo


 99 continue
end program

