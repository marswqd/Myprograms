program backgroundnoise
!-----
! �˳���������ȡ�����沨���ٶ�Ƶɢ��Ϣ      
! ���ߣ����嶫�����09�У������������ѧ
!
! ����˵����
! sacnum  I*4  ��խ���˲��õ���SAC�ļ�����
! NPTS    I*4  SACʱ�������ļ��Ĳ�������
! LN      I*4  ��ȡ��SACʱ�������ļ��Ĳ�������
! rhdr    R*4  SAC�ļ�ͷʵ�ͱ���
! ihdr    I*4  SAC�ļ�ͷ���ͱ���
! chdr    C*   SAC�ļ�ͷ�ַ��ͱ���
! sacname C*   ��խ���˲��õ���SAC�ļ��� 
! tp      R*4  խ���˲���������
! t0,v0   R*4  ���������ģ�͵��������Ӧ����
! NPTS,DT,B,O,DIST  SAC�ļ�ͷ����:��������,�������,��һ��������,����ʱ��,���о�
! sacdata R*4  SAC�ļ������Ϣ
! tt      R*4  ����������ʱ
! wv      R*4  ���������Ӧ����
! sacamp  R*4  SAC�ļ���һ�������Ϣ
! pv      R*4  �õ���Ƶɢ��Ϣ
! sacpv   R*4  Ƶɢ��Ϣ����
! tb,vb   I*4  ��������Ƶɢ��
!
! �ļ�˵����
! controlfile  �����ļ�����һ��Ϊ���������ģ�͵��������Ӧ���� t0,v0
!                        �ڶ���Ϊ�ļ�����
!                        �Ե�����Ϊ����Ϊ�ļ����Ͷ�Ӧխ���˲��������� sacname/tp
!
! �ӳ���˵����
! brsac     ��ȡSAC�������ļ���Ϣ
! band      ��������������۵���ӽ���Ƶɢ��
! search    ����ȫ����Ƶɢ��
!
!
!
!
!-----
 integer*4::i,j,k,tb,vb,bb
 integer*4::sacnum,NPTS
 real*4::t0,v0
 real*4::B,O,DT,DIST
 real*4,allocatable::tp(:),tt(:),wv(:),pv(:,:)
 real*4,allocatable::sacdata(:,:),sacpv(:,:)
 character*20,allocatable::sacname(:)
 real*4,parameter::author=87.8168
 
 integer*4,parameter::LN=10000
 real*4::data(LN)
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)
 
 open(10,file='controlfile',status='old')
 read(10,*) t0,v0
 read(10,*) sacnum
 print*, sacnum
 allocate(sacname(sacnum))
 allocate(tp(sacnum))
 do i=1,sacnum
  read(10,*) sacname(i)
  !print*, sacname(i)
  sacname(i)=trim(adjustl(sacname(i)))
  !print*, sacname(i)
  read(10,*) tp(i)
 enddo 
 close(10) 

 i=1
 call brsac(11,LN,sacname(i),data,nerr)
 NPTS=ihdr(10)
 DT=rhdr(1)
 B=rhdr(6)
 O=rhdr(8)
 DIST=rhdr(51)
 print*, NPTS,DT,B,O,DIST
 allocate(sacdata(0:NPTS,0:sacnum))
 allocate(tt(NPTS))
 allocate(wv(NPTS))
 sacdata(0,0)=author
 do i=1,sacnum
  sacdata(0,i)=tp(i)
 enddo
 do i=1,NPTS
  tt(i)=(B-O)+(i-1)*DT
  wv(i)=DIST/tt(i)
  sacdata(i,0)=wv(i)
 enddo
 
 do i=1,sacnum
  j=i+10
  call brsac(j,LN,sacname(i),data,nerr)
  do k=1,NPTS
   sacdata(k,i)=data(k)
  enddo
  !print*, rhdr(3)
 enddo
 
 open(600,file='sacdata')
 do i=0,NPTS
  write(600,*) sacdata(i,:)
 enddo
 close(600)
 
 allocate(pv(sacnum,2))
 allocate(sacpv(0:NPTS,0:sacnum))
 sacpv=sacdata
 sacpv(1:NPTS,1:sacnum)=0.0
 call pointing(sacdata,sacpv,NPTS,sacnum)
 call band(tp,sacnum,t0,tb)
 call band(tt,NPTS,DIST/v0,vb)
 print*, tb,vb
 bb=vb
 do k=tb,1,-1
  call search(sacpv,NPTS,sacnum,k,vb)
  print*, k,vb
  print*, sacpv(vb,k)
  pv(k,2)=wv(vb)
  pv(k,1)=tp(k)
  sacpv(vb,k)=author
 enddo
 do k=tb+1,sacnum
  call search(sacpv,NPTS,sacnum,k,bb)
  print*, k,bb
  print*, sacpv(bb,k)
  pv(k,2)=wv(bb)
  pv(k,1)=tp(k)
  sacpv(bb,k)=author
 enddo 

 open(700,file='sacpv')
 open(800,file='pv')
 do i=0,NPTS
  write(700,*) sacpv(i,:)
 enddo
 do i=1,sacnum
  write(800,*) pv(i,:)
 enddo
 close(700)
 close(800)
 
end program


subroutine brsac (IRU,LN,name,data,nerr)
!-----
!       IRU I*4 logical unit for IO
!       LN  I*4 length of data array
!       name    C*  Name of file to be opened
!       rhdr    R*4 Real header
!       ihdr    I*4 Integer Header
!       chdr    C*  Character Header
!       data    R*4 Array of trace values
!       nerr    I*4 -1 file does not exist
!               -2 data points in file exceed dimension
!
!       NOTE IF THE BINARY FILE HAS MORE THAN LN POINTS, THEN ONLY
!       LN POINTS ARE USED
!-----
!
!  This routine reads waveform data written in SAC binary format.
!
!  Written by Hafidh A. A. Ghalib, 1988.
!
!-----
 real*4::data(LN)
 logical::ext

 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)
 character*(*)::name
!-----
!  Read real and integer header blocks to find actual number
!  of waveform data points which is stored in ihdr(10).
!
!-----
! print*, name
 inquire(file=name,exist=ext)
 if(.not. ext) then
  ihdr(10)=0
  nerr=-1
  return
 endif
 nerr = 0
 
 open(IRU,file=name,form='unformatted',access='direct',recl=440,status='old')
 read(IRU,rec=1) (rhdr(i), i=1,70),(ihdr(i), i=1,40)
 close(IRU)
!-----
!
!  Read header and waveform data blocks using recored 
!           length of 158*4=632.
!
!-----
 if(ihdr(10).gt.LN) then
  maxpts=LN
  ihdr(10)=LN
  nerr=-2
 else 
  maxpts=ihdr(10)
  nerr=0
 endif
!-----
 nbytes=632+4*maxpts
 nread=0
!-----
!       because of SUNOS Fortran problems with IO transfers 
!       more than 2048 bytes, read these  chunks in 
!----- 
 ndat=maxpts
 if(nbytes.gt.2048) then
  open(IRU,file=name,form='unformatted',access='direct',recl=2048)
  ndat1=(2048-632)/4
  irec=1
  read(IRU,rec=irec,err=1001) (rhdr(i),i=1,70),   &
                              (ihdr(i),i=1,40),   &
                              (chdr(i),i=1,24),   &
                              (data(i),i=1,ndat1)
  nread=nread+ndat1
  1000 continue
  nl=nread+1
  nh=nl+512-1
  if(nh.gt.ndat) then
   nh=ndat
  endif
  if(nl.gt.ndat) goto 1001
  irec=irec+1
  read(IRU,rec=irec,err=1001) (data(i),i=nl,nh)
  nread=nread+(nh-nl+1)

  goto 1000
  1001 continue
  close(IRU)
 else
  open(IRU,file=name,form='unformatted',access='direct',recl=nbytes)
  read(IRU,rec=1) (rhdr(i),i=1,70),   &
                  (ihdr(i),i=1,40),   &
                  (chdr(i),i=1,24),   &
                  (data(i),i=1,ndat)
  close(IRU)
 endif
 if(ihdr(10).gt.LN) then
  maxpts=LN
  ihdr(10)=LN
 else 
  maxpts=ihdr(10)
 endif
 ihdr(10)=maxpts
 return
end


subroutine band(x,m,x0,lb)
 real*4::x(m),a,b,c
 integer*4::lb
 a=x(1)-x0
 c=0.0
 do i=1,m-1
  b=x(i+1)-x0
  if(a*b.le.c) then
   if(abs(a).gt.abs(b)) then
    lb=i+1
   else
    lb=i
   return
   endif
  endif
  a=b
 enddo
end
  

subroutine search(x,m,n,xb,yb)
 real*4::x(0:m,0:n),a,b
 integer*4::xb,yb
 do i=yb,1,-1
  a=x(i,xb)
  if(a==1.0) exit
 enddo
 do j=yb,m
  b=x(j,xb)
  if(b==1.0) exit
 enddo
 if(i==1) then
  yb=j 
  return
 endif
 if(j==m) then
  yb=i
  return
 endif
 if(abs(yb-i).le.abs(j-yb)) then
  yb=i
 else
  yb=j
 endif
end


subroutine pointing(x,y,m,n)
 real*4::x(0:m,0:n),y(0:m,0:n),a,b,c
 c=0.0 
 do j=1,n
  a=x(2,j)-x(1,j)
  y(1,j)=0.0
  y(m,j)=0.0
  do i=2,m-1
   b=x(i+1,j)-x(i,j)
   if(a.ge.c.and.b.le.c) then
    y(i,j)=1.0
   else
    y(i,j)=0.0
   endif
   a=b
  enddo
 enddo 
end