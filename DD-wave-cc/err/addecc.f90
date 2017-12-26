!功能：读取随机误差文件和数据文件，生成添加误差的数据文件
!   #  id1   id2   d12
!   sta  tt(or tt1 tt2)  weight  phase
!作者：王清东   日期：2015/9/21 9:11:59
program addecc
 implicit none

 integer*4::i,j,k,id1,id2,nep,nline,ndata,num,m,n
 integer*4,allocatable::line(:)
 real*4::d12,tt1,tt2,w
 real*4,allocatable::tt(:),weight(:),err(:,:)
 character::flag*1,card*200,datafile*50,errfile*50,name*50
 character,allocatable::sta(:)*7,phase(:)*1,ep(:)*50


 !open(9,file='addecc.log')

 open(10,file='addecc.in',status='old')
 read(10,*) num
 read(10,*) datafile
 read(10,*) errfile
 close(10)


 print*, 'Step 1: read data file'
 open(11,file=trim(datafile),status='old',action='read')  !输入地震对走时数据(块)
 call filelen(11,n)
 allocate(ep(n),line(n),sta(n),tt(n),weight(n),phase(n))
 i=0
 j=0
 k=0
 do
  read(11,'(a)',end=20) card
  i=i+1
  read(card,*) flag
  if(flag=='#') then  !找到事件标示
   j=j+1
   ep(j)=trim(card)
   line(j)=i
  else
   k=k+1
   read(card,*) sta(k),tt(k),weight(k),phase(k)
  endif
 enddo
 20 continue
 nep=j
 ndata=k
 nline=i
 close(11)


 print*, 'Step 2: read error file'
 open(12,file=trim(errfile),status='old',action='read')
 call filelen(12,m)
 if(m/=ndata) then
  print*, 'error in data num compare: ',ndata,m
 endif
 allocate(err(ndata,num))
 do i=1,ndata
  read(12,*) (err(i,j),j=1,num)
 enddo
 close(12)


 print*, 'Step 3: creat errdata file'
 w=1.0e0
 do i=1,num
  call namegen(trim(datafile),'000',i,name)
  print*, trim(name)
  open(13,file=trim(name))
  do j=1,nep-1
   write(13,'(a)') trim(ep(j))
   do k=line(j)-(j-1),line(j+1)-(j+1)
    !write(13,*) sta(k),tt(k)+err(k,i),weight(k),phase(k)
    write(13,*) sta(k),tt(k)+err(k,i),w,phase(k)
   enddo
  enddo
  write(13,'(a)') trim(ep(nep))
  do k=line(nep)-(nep-1),ndata
   !write(13,*) sta(k),tt(k)+err(k,i),weight(k),phase(k)
   write(13,*) sta(k),tt(k)+err(k,i),w,phase(k)
  enddo
  close(13)
 enddo


 99 continue
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


subroutine nval(line,nv)
! get number of values, separated by spaces, in a given line
 implicit none
 character*(*)::line
 integer*4::nv,j,k,trimlen
 j=0
 trimlen=len_trim(adjustl(line))
 do k=2,trimlen
  if(line(k:k).ne.' '.and.line(k-1:k-1).eq.' ') j=j+1
 enddo
 if(line(1:1).ne.' ') j=j+1
 nv=j
end subroutine


!subroutine namegen(prename,num,index,ext,name)
subroutine namegen(prename,num,index,name)
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
 !character*(*)::prename,name,ext,num
 character*(*)::prename,name,num
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
 !name=trim(prename)//trim(num1)//'.'//trim(ext)
 name=trim(prename)//trim(num1)
end subroutine
