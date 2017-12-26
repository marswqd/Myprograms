!功能： 读入已知的台站经纬度信息，提取出地震目录中的台站的信息
!作者：王清东   时间：2015/8/10 20:16:54
program fsta
 implicit none
 integer*4::i,j,k,nsta,nn
 !real*4::
 real*4,allocatable::stla(:),stlo(:),stel(:)
 character::stalog*60,stainfo*60,staout*60
 character,allocatable::sta(:)*7,st(:)*7

 !参数设定
 stalog='logsta09-13.txt'    !要寻找的台站
 stainfo='sta09-13.txt'  !现有的台站信息
 staout='fsta.txt'       !找到的台站


 open(10,file=stalog,status='old')
 call filelen(10,nsta)
 allocate(sta(nsta))
 do i=1,nsta
  read(10,*) sta(i)
 enddo
 close(10)

 open(10,file=stainfo,status='old')
 call filelen(10,nn)
 allocate(st(nn),stla(nn),stlo(nn),stel(nn))
 do i=1,nn
  read(10,*) st(i),stla(i),stlo(i),stel(i)
 enddo
 close(10)

 open(10,file=staout)
 open(11,file='fsta.log')
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
