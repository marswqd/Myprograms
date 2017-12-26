!功能： 找到地震目录中的台站
!作者：王清东   时间：2015/8/10 20:16:54
program logsta
 implicit none
 integer*4::i,j,k,nsta,nn
 real*4,allocatable::stla(:),stlo(:),stel(:)
 character::sta(500)*7,ctmp*7,flag*1,line*100
 character::catalog*60,stainfo*60,staout*60
 character,allocatable::st(:)*7


 !参数设定
 catalog='log_jg.txt'    !输入地震目录 ph2dt型
 stainfo='sta09-14.txt'  !现有的台站信息
 staout='logsta_jg.txt'       !找到的台站

 open(10,file=stainfo,status='old')
 call filelen(10,nn)
 allocate(st(nn),stla(nn),stlo(nn),stel(nn))
 do i=1,nn
  read(10,*) st(i),stla(i),stlo(i),stel(i)
 enddo
 close(10)

 open(10,file=catalog,status='old')
 nsta=1
 do
  read(10,'(a100)',end=20) line
  read(line,*) flag
  if(flag=='#') cycle
  read(line,*) sta(nsta)
  exit
 enddo
 !write(11,'(a)') trim(sta(nsta))
 do
  read(10,'(a100)',end=20) line
  read(line,*) flag
  if(flag=='#') cycle
  read(line,*) ctmp
  k=0
  do j=1,nsta
   if(trim(ctmp)==trim(sta(j))) then
    k=1
    exit
   endif
  enddo
  if(k==1) cycle
  nsta=nsta+1
  sta(nsta)=ctmp
  !write(11,'(a)') trim(sta(nsta))
 enddo
 20 continue
 close(10)
 close(11)

 open(10,file=staout)
 open(11,file='logsta.log')
 do i=1,nsta
  k=0
  do j=1,nn
   if(trim(sta(i))==trim(st(j))) then
    write(10,*) st(j),stla(j),stlo(j),stel(j)
    k=1
    exit
   endif
  enddo
  if(k==1) cycle
  print*, 'unknown station: ',trim(sta(i))
  write(11,*) 'unknown station: ',trim(sta(i))
 enddo
 close(10)
 close(11)


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

