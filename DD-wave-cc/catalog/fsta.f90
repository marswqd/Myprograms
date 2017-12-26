!���ܣ� ������֪��̨վ��γ����Ϣ����ȡ������Ŀ¼�е�̨վ����Ϣ
!���ߣ����嶫   ʱ�䣺2015/8/10 20:16:54
program fsta
 implicit none
 integer*4::i,j,k,nsta,nn
 !real*4::
 real*4,allocatable::stla(:),stlo(:),stel(:)
 character::stalog*60,stainfo*60,staout*60
 character,allocatable::sta(:)*7,st(:)*7

 !�����趨
 stalog='logsta09-13.txt'    !ҪѰ�ҵ�̨վ
 stainfo='sta09-13.txt'  !���е�̨վ��Ϣ
 staout='fsta.txt'       !�ҵ���̨վ


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
!�õ��ļ�����
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
