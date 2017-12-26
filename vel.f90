!
program vel
 implicit none

 integer*4::i,j,k,num,IRU
 real*4::T1,T2
 character*9::wname
 character*60,allocatable::sacname(:)

 character*5::flag
 character*1::rl,uc
 integer*4::mode
 real*4::tp,v,error,dist,az,samp,elat,elon,slat,slon

 call CPU_TIME(T1)
 open(8,file='info.txt')                   !����������Ϣ�ļ�

!��ȡ�����ļ���Ϣ
 open(9,file='velfile',status='old')       !velfile�д�Ų�̨ͬվͬһ�������
 read(9,*) num                             !num:�����sac�ļ���
 print*,num
 allocate(sacname(num))                    !sacname:sac�ļ���
 do i=1,num
  read(9,*) sacname(i)
  sacname(i)=adjustl(sacname(i))
  !print*, trim(sacname(i))
 enddo
 close(9)

 wname(1:7)='veldata'
 do i=10,20
  write(wname(8:9),'(i2)') i
  open(i,file=wname)
 enddo

 IRU=100
 do i=1,num
  open(IRU,file=sacname(i))
  print*, 'flag=',i
  print*, trim(sacname(i))
  101 read(IRU,*,end=102) flag,rl,uc,mode,tp,v,error,dist,az,samp,elat,elon,slat,slon
   do k=20,10,-1
    if(tp==k*1.0e0) then
     write(k,*) elat,elon,slat,slon,v,'1'
    endif
   enddo
  goto 101
  102 continue
  close(IRU)
 enddo

 99 continue
 deallocate(sacname)
 call CPU_TIME(T2)
 print*, 'T=',T2-T1
end program


