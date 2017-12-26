! �沨Ƶɢ�ٶȶ�ά�ֲ����ݳ��򡪡�����ģ��Tarantola���Է��ݽ�ѧ��ʾ����
! �����д�ˣ�������
! �人��ѧ ��������ϵ lbzhu@sgg.whu.edu.cn
! 2011��8��3��
! �ο����ף������������ݽ���-Tarantola�����Է��ݡ��Լ������򸽼����沨Ƶɢ��ά�ֲ��������ۡ�������������д��
  program tarantola_linear                 ! ��᣶�Ϊģ��
    integer*4,parameter::npoint=200
	  dimension::fx(npoint),fy(npoint)       ! �������顣
	  dimension::point(2,npoint)             ! ������ɢ��(�켣)�ľ�γ��ֵ��
	  character*50::dataname
    character*8::mydate
    character*10::mytime

	  allocatable::S(:,:),S_1(:,:)           ! S��������S_1��
	  allocatable::w(:,:),wt(:),wtt(:)
	  allocatable::rayvel(:,:),rayvel2(:,:)  ! ���߹켣����ٶȡ�
	  allocatable::rayp1(:,:),rayp2(:,:)     ! ÿ�����߹켣�ľ�γ�ȡ�
	  allocatable::Nid(:)                    ! ÿ�����ߵĹ켣�������
	  allocatable::dist(:)                   ! ÿ�����ߵĴ�Բ�����롣
	  allocatable::tt(:),dv(:),tt0(:),t0(:)
	  allocatable::alonlat(:,:)              ! ����������յ�ľ�γ��ֵ��

	  open( 7,file='keyparameter.in')
	  open( 8,file='keyparameter.out')
	  open(10,file='cm0pgpm.dat',form='unformatted')
 	  open(11,file='trace.dat',form='unformatted')
 	  open(12,file='plottrace.bln')
	  open(15,file='dt.dat')
	  open(16,file='constant.dat',form='unformatted')

    call date_and_time(mydate,mytime); write(8,*) 'Begine ! ',mydate,' ',mytime

    read(7,*) dataname   ! ʵ��Ƶɢ�����ļ������50���ַ���
	  read(7,*) aleng      ! ������س��ȡ�
	  read(7,*) cd0        ! �۲����ݣ���ʱ����ͳ�Ʊ�׼ƫ�
	  read(7,*) dg         ! ���߹켣������ɢ������Ľǣ��ȣ���dg�Ĵ�Сֱ��Ӱ������Ч�ʣ��ɴ��¶�Ϊ�ֱ��ʵ�һ�롣

	  open(9,file=dataname)
	  read(9,*) nnray,v0,sigma ! ������, ����ƽ���ٶ�, �ٶȾ����

	  allocate( alonlat(4,nnray) )
  	allocate( S(nnray,nnray),S_1(nnray,nnray) )              ! S��������S_1��
    allocate( w(nnray,2*nnray),wt(nnray),wtt(nnray) )
	  allocate( rayvel(npoint,nnray),rayvel2(npoint,nnray) )   ! ���߹켣���ģ��ֵ��
	  allocate( rayp1(npoint,nnray),rayp2(npoint,nnray) )      ! ÿ�����߹켣�ľ�γ�ȡ�
	  allocate( Nid(nnray) )           ! ÿ�����ߵĹ켣�������
	  allocate( dist(nnray) )          ! ÿ�����ߵĴ�Բ�����롣
	  allocate( tt(nnray),dv(nnray),tt0(nnray),t0(nnray) )

	  sigma2=(sigma/v0**2)**2  ! ����ģ�͵ķ��
	  cd2=cd0**2               ! ����ʱЭ�������ĶԽ�Ԫ�ظ���ֵ, �������ݣ���ʱ���ķ��
    PI=4.*ATAN(1.)           ! Բ���ʡ�
	  DR=PI/180.               ! �Ȼ����ȵı������ӡ�
	  dh=DR*dg*6371.           ! ��Ԫ��߳�
	  write(8,*) '�沨�ٶ�Ƶɢ�ֲ�����'
	  write(8,*) 'ʵ���ٶ�Ƶɢ�����ļ�����',dataname
	  write(8,*) '�沨·�������� ',nnray
	  write(8,*) '·��ƽ���ٶȣ�km/s���� ',v0
	  write(8,*) '·���ٶȾ���ƫ� ',sigma
	  write(8,*) '�۲����ݣ���ʱ���ı�׼���룩',cd0
	  write(8,*) '������س��ȣ�km����',aleng
	  write(8,*) '�沨·���켣������ɢ��������Ž�(��)��',dg
	  write(*,*) 'data flie: ',dataname

	  write(16) nnray,DR,dg,sigma,aleng,cd0,v0
! ����˵����nnray=����������DR=�Ȼ����ȣ�dg=���߹켣���ڵ����Ľǣ��ȣ���sigma=�ٶȾ����
! aleng=������س��ȣ�cd0=�۲����ݣ�������ʱ���ı�׼��v0=ʵ���ٶȵ�ƽ��ֵ��

	  do i=1,nnray
		  read(9,*) alonlat(1,i),alonlat(2,i),alonlat(3,i),alonlat(4,i),tt0(i) ! ������������յ�ľ�γ�ȣ��Լ�ʵ���ٶ�ֵ��
    enddo

!! �������Է��������ʵ��ƽ��Ƶɢ�ٶ�v0Ϊ�����ٶ�ģ�ͣ��䵹��Ϊ᣶�����ģ�ͣ���������sigmaΪ�����ٶ�ģ�͵ľ����
!! ע�⣺������س���=aleng���۲���ʱ�ı�׼���=cd0���Լ��ٶȾ�����sigma, ���߶���Ӱ�췴�ݵķֱ��ʼ�ģ�͵���


! �������߹켣�ľ�γ�ȡ�
    do i=1,nnray
		  a1=alonlat(1,i)
		  a2=alonlat(2,i)
		  b1=alonlat(3,i)
		  b2=alonlat(4,i)
		  call ALFA(DR,a1,a2,b1,b2,C)    ! �������߷�λ��C�����ȣ���
		  call TRACE(DR,a1,a2,b1,b2,C,dg,point,npoint,N)  ! �������߹켣����γ�ȣ���
	    Nid(i)=N                                        ! ��i�����ߵĹ켣������
		  dist(i)=DISTANCE(DR,a1,a2,b1,b2)                ! �������ߴ�Բ���ľ��롣
			do j=1,N
				rayp1(j,i)=point(1,j)
				rayp2(j,i)=point(2,j)
			enddo
! ��¼���߹켣
			write(11) N,dist(i),tt0(i)
			write(11) (point(1,j),point(2,j),j=1,N)      ! ��¼���߹켣
			write(12,*) N
			do j=1,N
				write(12,21) point(1,j),point(2,j)         ! ��¼���߹켣��
			enddo
	  enddo
21  format(1x,2f10.4)
! ���߹켣���������


	  do i=1,nnray                    
		  t0(i)=dist(i)/tt0(i)    ! �ٶ�����ת��Ϊ��ʱ����, �˴���tt0���ٶȣ�ʵ��ֵ����
	  enddo	
! �����ʼģ�͵�������ʱtt0��				
	  do i=1,nnray
		  tt0(i)=dist(i)/v0
	  enddo
	  
	  deallocate(alonlat)

    call date_and_time(mydate,mytime); write(8,*) '���߹켣�����������ʼ����S���� ',mydate,' ',mytime
	    

! ����S����       
	  do i1=1,nnray
		  do j1=1,Nid(i1)
			  do i2=1,nnray
				  do j2=1,Nid(i2)
					  DL=DISTANCE(DR,rayp1(j1,i1),rayp2(j1,i1),rayp1(j2,i2),rayp2(j2,i2))
						fx(j2)=gauss(sigma2,DL,aleng)            ! �ڻ��ֱ�������, ����Э����
			  	enddo
					c1=0.5*(fx(1)+fx(Nid(i2)-1))               ! �ݶȷ��ڻ���
					do j3=2,Nid(i2)-2
						c1=c1+fx(j3)
					enddo
					c1=c1*dh
					DL=DISTANCE(DR,rayp1(Nid(i2)-1,i2),rayp2(Nid(i2)-1,i2),rayp1(Nid(i2),i2),rayp2(Nid(i2),i2))
					c1=c1+0.5*DL*(fx(Nid(i2)-1)+fx(Nid(i2)))   ! �ڻ��ֽ���
					wt(i2)=c1           ! ��¼�ڻ��ֵ�ֵ��wt(i2)=��i1�������ϵĵ�j1���㣬����ڵ�i2�����߻��֡�
			  enddo
			  write(10) (wt(i3),i3=1,nnray)
		  enddo
    enddo
	  rewind(10)
! �����
	  do i1=1,nnray
			do j3=1,Nid(i1)
				read(10) (rayvel2(j3,i3),i3=1,nnray)   ! �����ڻ��֡�
			enddo
		  do i2=i1,nnray
			  do j1=1,Nid(i1)
					fy(j1)=rayvel2(j1,i2)                ! ����ֱ�������
			  enddo
				c1=0.5*(fy(1)+fy(Nid(i1)-1))           ! �ݶȷ������
				do j3=2,Nid(i1)-2
					c1=c1+fy(j3)
				enddo
				c1=c1*dh
				DL=DISTANCE(DR,rayp1(Nid(i1)-1,i1),rayp2(Nid(i1)-1,i1),rayp1(Nid(i1),i1),rayp2(Nid(i1),i1))
				c1=c1+0.5*DL*(fy(Nid(i1)-1)+fy(Nid(i1)))           ! ����ֽ���
				S(i1,i2)=c1
				S(i2,i1)=c1
				if(i1.eq.i2) S(i1,i1)=c1+cd2
		  enddo
    enddo
! S��������ɡ�


    call date_and_time(mydate,mytime); write(8,*) 'S��������ɡ� ',mydate,' ',mytime

		write(*,*) 'S array is formed!'

		itern=0
20	itern=itern+1
		if(itern.eq.2) then
! �ı����ݵķ�������¼���S����
			do i=1,nnray
				S(i,i)=S(i,i)-cd2			   ! �ȼ�ȥԭ����ƽ�����
			enddo
			rewind(15)
			read(15,*) ad
			do i=1,nnray
				read(15,*) itemp,cd0
				S(i,i)=S(i,i)+cd0**2     ! �Խ�Ԫ�ؼ����µ����ݷ��
			enddo
		endif
! ��S������棬S_1��
		call Jzqn1(S,nnray,S_1,w)

		write(*,*) 'S_1 array is formed!'

    call date_and_time(mydate,mytime); write(8,*) '�������ķ��ݽ⡣ ',mydate,' ',mytime

! �������ķ��ݽ�
	  rewind(10)
	  do i1=1,nnray
		  do j1=1,Nid(i1)
			  read(10) (wt(i2),i2=1,nnray)
        wtt=matmul(wt,S_1)					   ! ����ˡ�
		    wt=t0-tt0
        dv2=dot_product(wtt,wt)				 ! ʸ����(���)��
        rayvel2(j1,i1)=1./v0+dv2       ! �õ����ݺ��᣶�ģ�͡�
		  enddo
	  enddo
	  rewind(10)

    call date_and_time(mydate,mytime); write(8,*) '��ɷ��ݽ�ļ��㡣 ',mydate,' ',mytime

! �洢�����ٶ�ģ�͡�
		if(itern.eq.1) then
			open(13,file='vel.dat')				 ! ��ƽ����ʱ�������Ϊ�������ݷ���ķ����ٶ�ģ�͡�
			do i1=1,nnray
				do j1=1,Nid(i1)
					write(13,22) rayp1(j1,i1),rayp2(j1,i1),1./rayvel2(j1,i1)
				enddo
			enddo
		elseif(itern.eq.2) then
			open(18,file='vel2.dat')			 ! ����ʱƫ��ΪȨ�صķ����ٶ�ģ�͡�
			do i1=1,nnray
				do j1=1,Nid(i1)
					write(18,22) rayp1(j1,i1),rayp2(j1,i1),1./rayvel2(j1,i1)
				enddo
			enddo
		endif
22	format(1x,3f12.4)

! ���㷴��ģ�͵�������ʱ����ʵ����ʱ��ƫ�
		do i1=1,nnray
			do j1=1,Nid(i1)
				fx(j1)=rayvel2(j1,i1)            ! �ڻ��ֱ�����������᣶ȣ�
			enddo
			c1=0.5*(fx(1)+fx(Nid(i1)-1))       ! �ݶȷ��ڻ���
			do j3=2,Nid(i1)-2
				c1=c1+fx(j3)
			enddo
			c1=c1*dh
			DL=DISTANCE(DR,rayp1(Nid(i1)-1,i1),rayp2(Nid(i1)-1,i1),rayp1(Nid(i1),i1),rayp2(Nid(i1),i1))
			tt(i1)=c1+0.5*DL*(fx(Nid(i1)-1)+fx(Nid(i1)))  ! �ڻ��ֽ������õ���i1��·������ģ�͵�������ʱ��
			dv(i1)=abs(t0(i1)-tt(i1))		       ! ������ʵ��ľ�����ʱ�
		enddo
! ������ʱ����ʱ����������

! �洢��ʱ����������ʱƫ��
		ad=0.
		do i=1,nnray
			ad=ad+dv(i)**2
		enddo
		ad=sqrt(ad/nnray)  ! ������

		write(15,*) ad
		do i=1,nnray
			write(15,23) i,dv(i)
		enddo
23	format(1x,i6,f10.4)


		if(itern.eq.2) then
 ! �洢S_1�����
			open(17,file='S_1array.dat',form='unformatted')
 			do i=1,nnray    
				write(17) (S_1(i,j),j=1,nnray)
			enddo

! �������ֲ�ͼ��
			rewind(10)
			do i1=1,nnray
				do j1=1,Nid(i1)
					read(10) (wt(i2),i2=1,nnray)
					wtt=matmul(wt,S_1)
					dv2=dot_product(wtt,wt)
					rayvel(j1,i1)=(1./rayvel2(j1,i1))**2*sqrt(sigma2-dv2)   ! �õ��ٶ�ģ�ͱ�׼���ֵ��
				enddo
			enddo

! �洢���ֲ�
			open(14,file='dvel.dat')
			do i1=1,nnray
				do j1=1,Nid(i1)
					write(14,24) rayp1(j1,i1),rayp2(j1,i1),rayvel(j1,i1)    ! �洢�����ٶ�ģ�����ֲ�ͼ��
				enddo
			enddo
		endif
24	format(1x,3f10.4)

		if(itern.eq.1) goto 20
 
    call date_and_time(mydate,mytime); write(8,*) '��������� ',mydate,' ',mytime

999	end program

! ���������Ĵ�Բ������
! �����Ĵ�Բ�����뼰���Ľ�
  function DISTANCE(DR,f1,s1,f2,s2)
	  sita1=(90.-s1)*DR
	  sita2=(90.-s2)*DR
	  fin1=f1*DR
	  fin2=f2*DR
	  D=sin(sita1)*cos(fin1)*sin(sita2)*cos(fin2) &
	  &+sin(sita1)*sin(fin1)*sin(sita2)*sin(fin2) &
    &+cos(sita1)*cos(sita2)
    if(abs(D-1.).lt.1.0e-5) D=1.
	  af=acos(D)             ! ���Ľ�
	  Distance=af*6371.0     ! ����
	end function

	function gauss(sigma2,dis,aleng)      ! ģ������Э���������˹������alengΪ�ο��ֱ�߶ȡ�
		al=dis/aleng
		gauss=sigma2*exp(-0.5*al*al)
	end function


!�����ӳ���
  subroutine Jzqn1(ZA,N,ZAF,G)
!	  IMPLICIT REAL*8 (A-H,P-Z)
    DIMENSION ZA(N,N),ZAF(N,N),G(N,N+N)
	  DO 10 K=1,N
	    DO 20 J=1,N
	      G(K,J)=ZA(K,J)
	      G(K,J+N)=0.0
	      IF(J.EQ.K) G(K,J+N)=1
20    CONTINUE
10  CONTINUE
    DO 30 K=1,N
	    DO 40 I=K,N
	      IF(G(I,K).NE.0.0) GOTO 50
40    CONTINUE
      RETURN
50    DO 60 J=K,2*N
        B=G(K,J)
	      G(K,J)=G(I,J)
	      G(I,J)=B
60    CONTINUE
      C=1.0/G(K,K)
	    DO 70 J=K,2*N
	      G(K,J)=C*G(K,J)
70    CONTINUE
      DO 80 I=1,N
	      IF(I .NE. K) THEN
	        C=-G(I,K)
	        DO 90 J=K,2*N
	          G(I,J)=G(I,J)+C*G(K,J)
90        CONTINUE
        ENDIF
80    CONTINUE
30  CONTINUE
    DO 100 I=1,N
	    DO 110 J=1,N
	      ZAF(I,J)=G(I,J+N)
110   CONTINUE
100 CONTINUE
	  RETURN  
  END SUBROUTINE


! �������ǣ���֪���ߦ����¼�һ��C�����A���ο�������������硶��ѧ�ֲᡷ50��51ҳ��

  SUBROUTINE ALFA(DR,alon1,alat1,alon2,alat2,A)
! alon1,alat1Ϊ��һ����ľ�γ�ȣ�alon2,alat2Ϊ�ڶ�����ľ�γ�ȡ�
	  if((alat1.eq.0.).and.alat2.eq.0.) then     ! ����ͬ�ڳ���ϡ�
	    A=2.*atan(1.)
		  return
	  endif
	  PI=4.*ATAN(1.)
    ALF=(90.-alat2)*DR
    BTA=(90.-alat1)*DR
    C=abs((alon2-alon1)*DR)
    IF(C.lt.1.0e-5) THEN         ! ������ͬһ�����ϡ�
		  A=4.*ATAN(1.)
		  IF(ALF.LT.BTA) A=0.
      RETURN
    ENDIF
    TAB=COS((ALF-BTA)*.5)/COS(.5*(ALF+BTA))/TAN(C*.5)
    TAC=SIN((ALF-BTA)*.5)/SIN(.5*(ALF+BTA))/TAN(C*.5)
    TAB=ATAN(TAB)
    TAC=ATAN(TAC)
    A=TAB+TAC
	  if(A.lt.0.) A=PI+A
    RETURN
  END SUBROUTINE

! �ӳ��� TRACE ������������֮���Բ���Ĺ켣����γ�ȣ���
! �����������ǣ�������������֮���Բ���ϵ���ɢ�����꣬��������ɢ��֮������Ľ�Ϊdg(��)��
! ������ʽ����֪���ߦ����¼�һ��C�����B�ͱߦá����ӳ����еĽ�C��Ϊ�ӳ���ALFA���������A��
  SUBROUTINE TRACE(DR,alon1,alat1,alon2,alat2,C,dg,TNET,np,N)
    DIMENSION::TNET(2,np)
! alon1,alat1Ϊ��һ����ľ�γ�ȣ�alon2,alat2Ϊ�ڶ�����ľ�γ�ȡ�
    PI=4.*ATAN(1.)
	  TNET(1,1)=alon1
	  TNET(2,1)=alat1
	  i=1
	  if(C.eq.0.)then                    ! ������ͬһ�����ϡ�
 10   i=i+1
      at=alat1+dg*(i-1)
      if(at.ge.alat2) goto 200
		  TNET(1,i)=alon1
	    TNET(2,i)=at
		  goto 10
	  endif
	  if(C.eq.PI) then                   ! ������ͬһ�����ϡ�
 20   i=i+1
      at=alat1-dg*(i-1)
      if(at.le.alat2) goto 200
		  TNET(1,i)=alon1
	    TNET(2,i)=at
		  goto 20
	  endif

    ALF=(90.-alat1)*DR
	  i=1
100 i=i+1
    BTA=(dg*(i-1))*DR
    TAB=COS((ALF-BTA)*.5)/COS(.5*(ALF+BTA))/TAN(C*.5)
    TAC=SIN((ALF-BTA)*.5)/SIN(.5*(ALF+BTA))/TAN(C*.5)
    TAB=ATAN(TAB)
	  if(TAB.lt.0.) TAB=PI+TAB
    TAC=ATAN(TAC)
    AB=.5*(ALF+BTA)
	  if(abs(AB-.5*PI).lt.1.e-5) then
			A=TAB+TAC
  		B=TAB-TAC
			gama=sin(ALF)*sin(C)/sin(A)
			gama=PI-asin(gama)
			write(*,*) 'Bug!'
			goto 101
	  endif
	  GAMA2=COS(TAB)*TAN(.5*(ALF+BTA))/COS(TAC)
 	  GAMA=2.*ATAN(GAMA2)        ! ��γ�����ȣ�
	  B=TAB-TAC
101	GAMAD=90.-GAMA/DR          ! γ�ȣ��ȣ�
	  if(alon2.gt.alon1) then
		  BD=alon1+B/DR            ! ���ȣ��ȣ�
		  if(BD.ge.alon2) goto 200
		  TNET(1,i)=BD
		  TNET(2,i)=GAMAD
		  goto 100
	  endif
	  if(alon2.lt.alon1) then
		  BD=alon1-B/DR            ! ���ȣ��ȣ�
		  if(BD.le.alon2) goto 200
		  TNET(1,i)=BD
		  TNET(2,i)=GAMAD
		  goto 100
	  endif
    write(*,*) i
200 TNET(1,i)=alon2
	  TNET(2,i)=alat2
	  N=i
    RETURN
  END SUBROUTINE
