! 面波频散速度二维分布反演程序——连续模型Tarantola线性反演教学演示程序。
! 程序编写人：朱良保
! 武汉大学 地球物理系 lbzhu@sgg.whu.edu.cn
! 2011年8月3日
! 参考文献：《地球物理反演讲义-Tarantola非线性反演》以及本程序附件‘面波频散二维分布反演理论’。（朱良保编写）
  program tarantola_linear                 ! 以幔度为模型
    integer*4,parameter::npoint=200
	  dimension::fx(npoint),fy(npoint)       ! 工作数组。
	  dimension::point(2,npoint)             ! 射线离散点(轨迹)的经纬度值。
	  character*50::dataname
    character*8::mydate
    character*10::mytime

	  allocatable::S(:,:),S_1(:,:)           ! S矩阵及其逆S_1。
	  allocatable::w(:,:),wt(:),wtt(:)
	  allocatable::rayvel(:,:),rayvel2(:,:)  ! 射线轨迹点的速度。
	  allocatable::rayp1(:,:),rayp2(:,:)     ! 每条射线轨迹的经纬度。
	  allocatable::Nid(:)                    ! 每条射线的轨迹点个数。
	  allocatable::dist(:)                   ! 每条射线的大圆弧距离。
	  allocatable::tt(:),dv(:),tt0(:),t0(:)
	  allocatable::alonlat(:,:)              ! 射线起点与终点的经纬度值。

	  open( 7,file='keyparameter.in')
	  open( 8,file='keyparameter.out')
	  open(10,file='cm0pgpm.dat',form='unformatted')
 	  open(11,file='trace.dat',form='unformatted')
 	  open(12,file='plottrace.bln')
	  open(15,file='dt.dat')
	  open(16,file='constant.dat',form='unformatted')

    call date_and_time(mydate,mytime); write(8,*) 'Begine ! ',mydate,' ',mytime

    read(7,*) dataname   ! 实测频散数据文件名，最长50个字符。
	  read(7,*) aleng      ! 先验相关长度。
	  read(7,*) cd0        ! 观测数据（走时）的统计标准偏差。
	  read(7,*) dg         ! 射线轨迹相邻离散点的球心角（度）。dg的大小直接影响计算的效率，可大致定为分辨率的一半。

	  open(9,file=dataname)
	  read(9,*) nnray,v0,sigma ! 射线数, 区域平均速度, 速度均方差。

	  allocate( alonlat(4,nnray) )
  	allocate( S(nnray,nnray),S_1(nnray,nnray) )              ! S矩阵及其逆S_1。
    allocate( w(nnray,2*nnray),wt(nnray),wtt(nnray) )
	  allocate( rayvel(npoint,nnray),rayvel2(npoint,nnray) )   ! 射线轨迹点的模型值。
	  allocate( rayp1(npoint,nnray),rayp2(npoint,nnray) )      ! 每条射线轨迹的经纬度。
	  allocate( Nid(nnray) )           ! 每条射线的轨迹点个数。
	  allocate( dist(nnray) )          ! 每条射线的大圆弧距离。
	  allocate( tt(nnray),dv(nnray),tt0(nnray),t0(nnray) )

	  sigma2=(sigma/v0**2)**2  ! 先验模型的方差。
	  cd2=cd0**2               ! 给走时协方差矩阵的对角元素赋初值, 即：数据（走时）的方差。
    PI=4.*ATAN(1.)           ! 圆周率。
	  DR=PI/180.               ! 度化弧度的比例因子。
	  dh=DR*dg*6371.           ! 单元格边长
	  write(8,*) '面波速度频散分布反演'
	  write(8,*) '实测速度频散数据文件名：',dataname
	  write(8,*) '面波路径总数： ',nnray
	  write(8,*) '路径平均速度（km/s）： ',v0
	  write(8,*) '路径速度均方偏差： ',sigma
	  write(8,*) '观测数据（走时）的标准误差（秒）',cd0
	  write(8,*) '先验相关长度（km）：',aleng
	  write(8,*) '面波路径轨迹相邻离散点的球心张角(度)：',dg
	  write(*,*) 'data flie: ',dataname

	  write(16) nnray,DR,dg,sigma,aleng,cd0,v0
! 常量说明：nnray=射线总数，DR=度化弧度，dg=射线轨迹相邻点球心角（度），sigma=速度均方差，
! aleng=先验相关长度，cd0=观测数据（射线走时）的标准误差，v0=实测速度的平均值。

	  do i=1,nnray
		  read(9,*) alonlat(1,i),alonlat(2,i),alonlat(3,i),alonlat(4,i),tt0(i) ! 读射线起点与终点的经纬度，以及实测速度值。
    enddo

!! 本程序以反演区域的实测平均频散速度v0为先验速度模型（其倒数为幔度先验模型），均方差sigma为先验速度模型的均方差。
!! 注意：先验相关长度=aleng，观测走时的标准误差=cd0，以及速度均方差sigma, 三者都会影响反演的分辨率及模型的误差。


! 计算射线轨迹的经纬度。
    do i=1,nnray
		  a1=alonlat(1,i)
		  a2=alonlat(2,i)
		  b1=alonlat(3,i)
		  b2=alonlat(4,i)
		  call ALFA(DR,a1,a2,b1,b2,C)    ! 计算射线方位角C（弧度）。
		  call TRACE(DR,a1,a2,b1,b2,C,dg,point,npoint,N)  ! 计算射线轨迹（经纬度）。
	    Nid(i)=N                                        ! 第i条射线的轨迹点数。
		  dist(i)=DISTANCE(DR,a1,a2,b1,b2)                ! 计算射线大圆弧的距离。
			do j=1,N
				rayp1(j,i)=point(1,j)
				rayp2(j,i)=point(2,j)
			enddo
! 记录射线轨迹
			write(11) N,dist(i),tt0(i)
			write(11) (point(1,j),point(2,j),j=1,N)      ! 记录射线轨迹
			write(12,*) N
			do j=1,N
				write(12,21) point(1,j),point(2,j)         ! 记录射线轨迹。
			enddo
	  enddo
21  format(1x,2f10.4)
! 射线轨迹计算结束。


	  do i=1,nnray                    
		  t0(i)=dist(i)/tt0(i)    ! 速度数据转换为走时数据, 此处的tt0是速度（实测值）。
	  enddo	
! 计算初始模型的理论走时tt0。				
	  do i=1,nnray
		  tt0(i)=dist(i)/v0
	  enddo
	  
	  deallocate(alonlat)

    call date_and_time(mydate,mytime); write(8,*) '射线轨迹计算结束，开始构造S矩阵。 ',mydate,' ',mytime
	    

! 构造S矩阵       
	  do i1=1,nnray
		  do j1=1,Nid(i1)
			  do i2=1,nnray
				  do j2=1,Nid(i2)
					  DL=DISTANCE(DR,rayp1(j1,i1),rayp2(j1,i1),rayp1(j2,i2),rayp2(j2,i2))
						fx(j2)=gauss(sigma2,DL,aleng)            ! 内积分被积函数, 先验协方差
			  	enddo
					c1=0.5*(fx(1)+fx(Nid(i2)-1))               ! 梯度法内积分
					do j3=2,Nid(i2)-2
						c1=c1+fx(j3)
					enddo
					c1=c1*dh
					DL=DISTANCE(DR,rayp1(Nid(i2)-1,i2),rayp2(Nid(i2)-1,i2),rayp1(Nid(i2),i2),rayp2(Nid(i2),i2))
					c1=c1+0.5*DL*(fx(Nid(i2)-1)+fx(Nid(i2)))   ! 内积分结束
					wt(i2)=c1           ! 记录内积分的值。wt(i2)=第i1条射线上的第j1个点，相对于第i2条射线积分。
			  enddo
			  write(10) (wt(i3),i3=1,nnray)
		  enddo
    enddo
	  rewind(10)
! 外积分
	  do i1=1,nnray
			do j3=1,Nid(i1)
				read(10) (rayvel2(j3,i3),i3=1,nnray)   ! 读入内积分。
			enddo
		  do i2=i1,nnray
			  do j1=1,Nid(i1)
					fy(j1)=rayvel2(j1,i2)                ! 外积分被积函数
			  enddo
				c1=0.5*(fy(1)+fy(Nid(i1)-1))           ! 梯度法外积分
				do j3=2,Nid(i1)-2
					c1=c1+fy(j3)
				enddo
				c1=c1*dh
				DL=DISTANCE(DR,rayp1(Nid(i1)-1,i1),rayp2(Nid(i1)-1,i1),rayp1(Nid(i1),i1),rayp2(Nid(i1),i1))
				c1=c1+0.5*DL*(fy(Nid(i1)-1)+fy(Nid(i1)))           ! 外积分结束
				S(i1,i2)=c1
				S(i2,i1)=c1
				if(i1.eq.i2) S(i1,i1)=c1+cd2
		  enddo
    enddo
! S矩阵构造完成。


    call date_and_time(mydate,mytime); write(8,*) 'S矩阵构造完成。 ',mydate,' ',mytime

		write(*,*) 'S array is formed!'

		itern=0
20	itern=itern+1
		if(itern.eq.2) then
! 改变数据的方差后重新计算S矩阵。
			do i=1,nnray
				S(i,i)=S(i,i)-cd2			   ! 先减去原来的平均方差。
			enddo
			rewind(15)
			read(15,*) ad
			do i=1,nnray
				read(15,*) itemp,cd0
				S(i,i)=S(i,i)+cd0**2     ! 对角元素加上新的数据方差。
			enddo
		endif
! 求S矩阵的逆，S_1。
		call Jzqn1(S,nnray,S_1,w)

		write(*,*) 'S_1 array is formed!'

    call date_and_time(mydate,mytime); write(8,*) '构造最后的反演解。 ',mydate,' ',mytime

! 构造最后的反演解
	  rewind(10)
	  do i1=1,nnray
		  do j1=1,Nid(i1)
			  read(10) (wt(i2),i2=1,nnray)
        wtt=matmul(wt,S_1)					   ! 矩阵乘。
		    wt=t0-tt0
        dv2=dot_product(wtt,wt)				 ! 矢量积(点乘)。
        rayvel2(j1,i1)=1./v0+dv2       ! 得到反演后的幔度模型。
		  enddo
	  enddo
	  rewind(10)

    call date_and_time(mydate,mytime); write(8,*) '完成反演解的计算。 ',mydate,' ',mytime

! 存储最终速度模型。
		if(itern.eq.1) then
			open(13,file='vel.dat')				 ! 以平均走时测量误差为先验数据方差的反演速度模型。
			do i1=1,nnray
				do j1=1,Nid(i1)
					write(13,22) rayp1(j1,i1),rayp2(j1,i1),1./rayvel2(j1,i1)
				enddo
			enddo
		elseif(itern.eq.2) then
			open(18,file='vel2.dat')			 ! 以走时偏差为权重的反演速度模型。
			do i1=1,nnray
				do j1=1,Nid(i1)
					write(18,22) rayp1(j1,i1),rayp2(j1,i1),1./rayvel2(j1,i1)
				enddo
			enddo
		endif
22	format(1x,3f12.4)

! 计算反演模型的理论走时及与实测走时的偏差。
		do i1=1,nnray
			do j1=1,Nid(i1)
				fx(j1)=rayvel2(j1,i1)            ! 内积分被积函数（新幔度）
			enddo
			c1=0.5*(fx(1)+fx(Nid(i1)-1))       ! 梯度法内积分
			do j3=2,Nid(i1)-2
				c1=c1+fx(j3)
			enddo
			c1=c1*dh
			DL=DISTANCE(DR,rayp1(Nid(i1)-1,i1),rayp2(Nid(i1)-1,i1),rayp1(Nid(i1),i1),rayp2(Nid(i1),i1))
			tt(i1)=c1+0.5*DL*(fx(Nid(i1)-1)+fx(Nid(i1)))  ! 内积分结束，得到第i1条路径的新模型的理论走时。
			dv(i1)=abs(t0(i1)-tt(i1))		       ! 理论与实测的绝对走时差。
		enddo
! 理论走时及走时差计算结束。

! 存储走时均方根和走时偏差
		ad=0.
		do i=1,nnray
			ad=ad+dv(i)**2
		enddo
		ad=sqrt(ad/nnray)  ! 均方根

		write(15,*) ad
		do i=1,nnray
			write(15,23) i,dv(i)
		enddo
23	format(1x,i6,f10.4)


		if(itern.eq.2) then
 ! 存储S_1逆矩阵。
			open(17,file='S_1array.dat',form='unformatted')
 			do i=1,nnray    
				write(17) (S_1(i,j),j=1,nnray)
			enddo

! 计算误差分布图。
			rewind(10)
			do i1=1,nnray
				do j1=1,Nid(i1)
					read(10) (wt(i2),i2=1,nnray)
					wtt=matmul(wt,S_1)
					dv2=dot_product(wtt,wt)
					rayvel(j1,i1)=(1./rayvel2(j1,i1))**2*sqrt(sigma2-dv2)   ! 得到速度模型标准误差值。
				enddo
			enddo

! 存储误差分布
			open(14,file='dvel.dat')
			do i1=1,nnray
				do j1=1,Nid(i1)
					write(14,24) rayp1(j1,i1),rayp2(j1,i1),rayvel(j1,i1)    ! 存储最终速度模型误差分布图。
				enddo
			enddo
		endif
24	format(1x,3f10.4)

		if(itern.eq.1) goto 20
 
    call date_and_time(mydate,mytime); write(8,*) '程序结束。 ',mydate,' ',mytime

999	end program

! 计算两点间的大圆弧距离
! 两点间的大圆弧距离及球心角
  function DISTANCE(DR,f1,s1,f2,s2)
	  sita1=(90.-s1)*DR
	  sita2=(90.-s2)*DR
	  fin1=f1*DR
	  fin2=f2*DR
	  D=sin(sita1)*cos(fin1)*sin(sita2)*cos(fin2) &
	  &+sin(sita1)*sin(fin1)*sin(sita2)*sin(fin2) &
    &+cos(sita1)*cos(sita2)
    if(abs(D-1.).lt.1.0e-5) D=1.
	  af=acos(D)             ! 球心角
	  Distance=af*6371.0     ! 距离
	end function

	function gauss(sigma2,dis,aleng)      ! 模型先验协方差函数，高斯函数。aleng为参考分辨尺度。
		al=dis/aleng
		gauss=sigma2*exp(-0.5*al*al)
	end function


!求逆子程序
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


! 球面三角，已知两边α，β夹一角C，求角A，参考人民教育出版社《数学手册》50－51页。

  SUBROUTINE ALFA(DR,alon1,alat1,alon2,alat2,A)
! alon1,alat1为第一个点的经纬度，alon2,alat2为第二个点的经纬度。
	  if((alat1.eq.0.).and.alat2.eq.0.) then     ! 两点同在赤道上。
	    A=2.*atan(1.)
		  return
	  endif
	  PI=4.*ATAN(1.)
    ALF=(90.-alat2)*DR
    BTA=(90.-alat1)*DR
    C=abs((alon2-alon1)*DR)
    IF(C.lt.1.0e-5) THEN         ! 两点在同一经线上。
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

! 子程序 TRACE 求球面上两点之间大圆弧的轨迹（经纬度）。
! 利用球面三角，求球面上两点之间大圆弧上的离散点坐标，两相邻离散点之间的球心角为dg(度)。
! 基本公式：已知两边α，β夹一角C，求角B和边γ。本子程序中的角C即为子程序‘ALFA’的输出角A。
  SUBROUTINE TRACE(DR,alon1,alat1,alon2,alat2,C,dg,TNET,np,N)
    DIMENSION::TNET(2,np)
! alon1,alat1为第一个点的经纬度，alon2,alat2为第二个点的经纬度。
    PI=4.*ATAN(1.)
	  TNET(1,1)=alon1
	  TNET(2,1)=alat1
	  i=1
	  if(C.eq.0.)then                    ! 两点在同一经线上。
 10   i=i+1
      at=alat1+dg*(i-1)
      if(at.ge.alat2) goto 200
		  TNET(1,i)=alon1
	    TNET(2,i)=at
		  goto 10
	  endif
	  if(C.eq.PI) then                   ! 两点在同一经线上。
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
 	  GAMA=2.*ATAN(GAMA2)        ! 极纬（弧度）
	  B=TAB-TAC
101	GAMAD=90.-GAMA/DR          ! 纬度（度）
	  if(alon2.gt.alon1) then
		  BD=alon1+B/DR            ! 经度（度）
		  if(BD.ge.alon2) goto 200
		  TNET(1,i)=BD
		  TNET(2,i)=GAMAD
		  goto 100
	  endif
	  if(alon2.lt.alon1) then
		  BD=alon1-B/DR            ! 经度（度）
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
