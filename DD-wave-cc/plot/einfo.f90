!功能: 分析原始地震目录,统计数据信息,输出参数,便于GMT作图.
!作者: 王清东  时间: 2014/5/17 16:27:19
program einfo
 implicit none

 integer*4::i,j,l,m,n,ns,ik
 integer*4::date,time,id,cid,yr,mo,dy,hr,mi
 integer*4,allocatable::idd(:),indx(:)
 real*4::sec,stla,stlo,stel,evla,evlo,dep,mmag,ddist,aaz,t,ee,ex,ey,ez,x1,y1,x2,y2,weight
 real*4,allocatable::elon(:),elat(:),slon(:),slat(:),sele(:),depth(:),mag(:),dist(:),az(:),tt(:),x(:),y(:)
 real*4::k(2),alpha,mintt,maxtt,mindist,maxdist
 real*4::minlon,maxlon,minlat,maxlat,menlon,menlat,mindep,maxdep,minmag,maxmag
 character*7,allocatable::sta(:)
 character::flag*1,phase*1,ssta*7,st*7,phafile*20,stafile*20,card*150


 open(9,file='einfo.log')

 !已改用gmt进行剖面投影,下列参数无效
 !ik=0 !ik=0: 剖面为震源群拟合直线(A)及其垂线(B); ik=else: 剖面为下面给出的断层直线(A)及其垂线(B);
      !两垂直剖面的焦点是震源群中心(menlon,menlat)
 !断层参数
 !x1=103.534;y1=27.22878
 !x2=104.0999;y2=27.78207
 !k(2)=(y2-y1)/(x2-x1)
 !k(1)=y1-k(2)*x1

!phase.txt
 call getarg(1,phafile) !输入地震目录
 open(10,file=phafile,status='old',action='read')
 call getarg(2,stafile) !输入台站信息文件
 open(11,file=stafile,status='old',action='read')
 call filelen(10,n)
 call filelen(11,ns)
 open(12,file='event.txt')  !输出事件信息,便于gmt绘图
 open(13,file='tt0.txt')
 i=1
 j=0
 l=0
 mintt=99999.0e0
 maxtt=-1.0e0
 mindist=99999.0e0
 maxdist=-1.0e0
 do while(i<=n)
  read(10,'(a)') card
  i=i+1
  if(card(1:1)=='#'.or.card(2:2)=='#') then
   read(card,*) flag,yr,mo,dy,hr,mi,sec,evla,evlo,dep,mmag,ee,ee,ee,id
   write(12,'(5i4,f7.2,2f9.3,2f7.2,i8)') yr,mo,dy,hr,mi,sec,evla,evlo,dep,mmag,id
   j=j+1
   cycle
  endif
  ddist=-999.0e0
  aaz=-999.0e0
  !print*, card
  read(card,*,err=50) ssta,t,weight,phase,ddist,aaz  !sta,tt,weight,phase,dist,az
  goto 51
  50 read(card,*) ssta,t,weight,phase  !sta,tt,weight,phase
  51 continue
  stla=-999.0e0
  stlo=-999.0e0
  stel=-999.0e0
  ez=-999.0e0
  do m=1,ns
   read(11,*) card
   call nval(card,i)
   if(i==4)
    read(card,*) st,ex,ey,ez  !sta,stla,stlo,stel
   elseif(i==4)
    read(card,*) st,ex,ey  !sta,stla,stlo
   endif
   if(ssta==st) then
    stla=ex
    stlo=ey
    stel=ez
    exit
   endif
  enddo
  rewind(11)
  if(ddist==-999.0e0.and.stla/=-999.0e0) then
   call GetDistAz(evla,evlo,stla,stlo,ddist,aaz)
  endif
  if(phase=='P') l=l+1
  if(mintt>=t) mintt=t
  if(maxtt<=t) maxtt=t
  if(ddist/=-999.0e0) then
   if(mindist>=ddist) mindist=ddist
   if(maxdist<=ddist) maxdist=ddist
  endif
  !if(ddist/=-999.0e0) then
   write(13,*) ssta,t,ddist,aaz,phase,id,stla,stlo,dep,stel  !sta,tt,dist,az,phase,id,stla,stlo,depth,stel
  !endif
 enddo
 close(10)
 write(9,*) '>> Catalog Info <<'
 write(9,*) 'TTime:  mintt  maxtt  mindist  maxdist'
 write(9,*) '     ',mintt,maxtt,mindist,maxdist
 write(9,*) 'Events:',j,'  Phase:',n-j,'  P-time:',l,'  S-time:',n-j-l

!按震相-震中距从小到大排序走时数据
 open(14,file='tt.txt') !输出震相走时信息,便于gmt绘图
 !sort P
 rewind(13)
 allocate(sta(l),tt(l),dist(l),az(l),idd(l),slat(l),slon(l),depth(l),sele(l),indx(l))
 i=1
 do while(i<=l)
  read(13,*) sta(i),tt(i),dist(i),az(i),phase,idd(i),slat(i),slon(i),depth(i),sele(i)
  if(phase=='P'.or.phase=='p') i=i+1
 enddo
 call indexx(l,dist,indx)
 do i=1,l
  write(14,'(a,3f10.3,3x,a,3x,i8,3f10.3)') sta(indx(i)),tt(indx(i)),dist(indx(i)),az(indx(i)), &
                                 & 'P',idd(indx(i)),slat(indx(i)),slon(indx(i)),depth(indx(i)),sele(indx(i))
 enddo
 deallocate(sta,tt,dist,az,idd,slat,slon,indx,depth,sele)
 !sotr S
 rewind(13)
 allocate(sta(n-j-l),tt(n-j-l),dist(n-j-l),az(n-j-l),slat(n-j-l),slon(n-j-l),    &
       &  idd(n-j-l),depth(n-j-l),indx(n-j-l),sele(n-j-l))
 i=1
 do while(i<=n-j-l)
  read(13,*) sta(i),tt(i),dist(i),az(i),phase,idd(i),slat(i),slon(i),depth(i),sele(i)
  if(phase=='S'.or.phase=='s') i=i+1
 enddo
 call indexx(n-j-l,dist,indx)
 do i=1,n-j-l
  write(14,'(a,3f10.3,3x,a,3x,i8,3f10.3)') sta(indx(i)),tt(indx(i)),dist(indx(i)),az(indx(i)), &
                                  & 'S',idd(indx(i)),slat(indx(i)),slon(indx(i)),depth(indx(i)),sele(indx(i))
 enddo
 deallocate(sta,tt,dist,az,idd,slat,slon,indx,depth,sele)
 close(13,status='delete')
 rewind(14)

!统计地震目录中含有的台站
 open(15,file='staCT.txt')  !输出台站信息,便于gmt绘图
 call filelen(14,n)
 allocate(sta(n),tt(n),dist(n),az(n),idd(n),slat(n),slon(n),indx(n),sele(n))
 read(14,*) sta(1),tt(1),dist(1),az(1),phase,idd(1),slat(1),slon(1),ex,sele(1)
 write(15,*) sta(1),slat(1),slon(1)
 i=2
 30 do
  read(14,*,end=101) sta(i),tt(i),dist(i),az(i),phase,idd(i),slat(i),slon(i) ,ex,sele(i)
  do j=1,i-1
   if(sta(i)==sta(j)) goto 30
  enddo
  write(15,*) sta(i),slat(i),slon(i),sele(i)
  i=i+1
 enddo
 101 deallocate(sta,tt,dist,az,idd,slat,slon,indx,sele)
 write(9,*) '>> Stations In Catalog <<'
 write(9,*) 'station unknow:'
 rewind(15)
 ns=i-1
 allocate(sta(ns),slat(ns),slon(ns),dist(ns),az(ns))
 i=1
 j=1
 do while(j<=ns)
  read(15,*) sta(i),slat(i),slon(i)
  j=j+1
  if(slat(i)==-999.0e0) then
   write(9,*) sta(i)
   cycle
  endif
  i=i+1
 enddo
 ns=i-1
 minlon=minval(slon(1:ns))
 maxlon=maxval(slon(1:ns))
 minlat=minval(slat(1:ns))
 maxlat=maxval(slat(1:ns))
 write(9,*) 'Stations: ',ns
 write(9,*) 'Station range:  minlon  maxlon  minlat  maxlat'
 write(9,'(a,4f9.3)') '             ',minlon,maxlon,minlat,maxlat

!事件 : event.txt
 rewind(12)
 call filelen(12,n)
 allocate(elat(n),elon(n),depth(n),mag(n),x(n),y(n))
 do i=1,n
  read(12,*) yr,mo,dy,hr,mi,sec,elat(i),elon(i),depth(i),mag(i),id
 enddo
 close(12)
 minlon=minval(elon)
 maxlon=maxval(elon)
 minlat=minval(elat)
 maxlat=maxval(elat)
 mindep=minval(depth)
 maxdep=maxval(depth)
 minmag=minval(mag)
 maxmag=maxval(mag)
 call mean(elon,n,menlon)
 call mean(elat,n,menlat)
 write(9,*) '>> Events Info <<'
 write(9,*) 'Events: ',n
 write(9,*) 'Event range:  minlon  maxlon  minlat  maxlat  mindep  maxdep  minmag  maxmag  menlon  menlat'
 write(9,'(a,6f9.3,2f5.1,2f9.3)') '           ',minlon,maxlon,minlat,maxlat,mindep,  &
                                              maxdep,minmag,maxmag,menlon,menlat
 !地震台至地震群质心距离统计
 rewind(15)
 do i=1,ns
  if(slat(i)==-999.0e0.and.slon(i)==-999.0e0) then
   dist(i)=-999.0e0
   az(i)=-999.0e0
  else
   call GetDistAz(menlat,menlon,slat(i),slon(i),dist(i),az(i))
  endif
  write(15,'(a,6f10.3)') sta(i),slat(i),slon(i),dist(i),az(i),menlat,menlon
 enddo
 close(15)
 !震中距统计
 mindist=minval(dist)
 maxdist=maxval(dist)
 write(9,*) 'Epicentral distance statistics: dist=',mindist,maxdist
 m=floor(maxdist/100)+1
 do j=1,m
  l=0
  do i=1,ns
   if(dist(i)<j*100.and.dist(i)>=(j-1)*100) l=l+1
  enddo
  write(9,*) j*100,l
 enddo
 !方位角统计
 !azmax=maxval(az)
 write(9,*) 'Azimuth statistics: <=500km'
 do j=1,360/45
  l=0
  do i=1,ns
   if(dist(i)<=500.0e0.and.az(i)<j*45.and.az(i)>=(j-1)*45) l=l+1
  enddo
   write(9,*) j*45,l
 enddo
 !震源深度统计
 write(9,*) 'Depth statistics: depth=',mindep,maxdep
 m=floor(maxdep/5)+1
 do j=1,m
  l=0
  do i=1,n
   if(depth(i)<j*5.and.depth(i)>=(j-1)*5) l=l+1
  enddo
  write(9,*) j*5,l
 enddo
 !震级统计
 write(9,*) 'magnitude statistics: mag=',minmag,maxmag
 m=floor(maxmag)+1
 do j=1,m
  l=0
  do i=1,n
   if(mag(i)<j.and.mag(i)>=(j-1)) l=l+1
  enddo
  write(9,*) j,l
 enddo


 goto 1000 !跳过以下代码
 !震源区直线拟合,确定两垂直剖面
 !剖面A的斜率是K(2),与X轴(经度)的顺时针夹角是alpha;剖面B与剖面A垂直,两者的交点是(menlon,menlat)
 open(16,file='proe.txt')
 if(ik==0) then
  call LSLF(elon,elat,n,k,ee,ex,ey,ez)
 endif
 call mean(elon,n,menlon)
 call mean(elat,n,menlat)
 if(abs(k(2))>0.0001e0) then
  k(1)=menlat-menlon*k(2)
  !k(1)=menlat+menlon/k(2)
  alpha=atan2(menlat-k(1),menlon)
  do i=1,n
   elon(i)=elon(i)-menlon !平移
   elat(i)=elat(i)-menlat
   x(i)=cos(alpha)*elon(i)+sin(alpha)*elat(i)  !旋转
   y(i)=cos(alpha)*elat(i)-sin(alpha)*elon(i)
   x(i)=x(i)*111.195 !rad to km
   y(i)=y(i)*111.195 !rad to km
   write(16,*) x(i),y(i),depth(i),mag(i)
  enddo
 else    !剖面A为水平线
  do i=1,n
   x(i)=elon(i)-menlon
   y(i)=elat(i)-menlat
   x(i)=x(i)*111.195 !rad to km
   y(i)=y(i)*111.195 !rad to km
   write(16,*) x(i),y(i),depth(i),mag(i)
  enddo
 endif
 close(16)
 minlon=minval(x)
 maxlon=maxval(x)
 minlat=minval(y)
 maxlat=maxval(y)
 write(9,*) '>> Vertical Profile Projection <<'
 write(9,*) 'Profile:  alpha  k   minX   maxX   minY   maxY   menlon  menlat'
 write(9,'(a,8f9.3)') '     ',alpha/3.1415926*180,k(2),minlon,maxlon,minlat,maxlat,menlon,menlat
 if(abs(k(2))>0.0001e0) then   !计算出剖面线的大致范围
  !A剖面
  x(1)=minlon/111.195*cos(alpha)+menlon
  y(1)=minlon/111.195*sin(alpha)+menlat
  x(2)=maxlon/111.195*cos(alpha)+menlon
  y(2)=maxlon/111.195*sin(alpha)+menlat
  !B剖面
  x(3)=minlat/111.195*sin(alpha)+menlon
  y(3)=minlat/111.195*cos(alpha)+menlat
  x(4)=maxlat/111.195*sin(alpha)+menlon
  y(4)=maxlat/111.195*cos(alpha)+menlat
 else
  !剖面A为水平线
  x(1)=minlon/111.195+menlon
  y(1)=menlat
  x(2)=maxlon/111.195+menlon
  y(2)=menlat
  !B剖面
  x(3)=menlon
  y(3)=minlat/111.195+menlat
  x(4)=menlon
  y(4)=maxlat/111.195+menlat
 endif
 write(9,*) 'ProfileA:  minlon   maxlon   minlat  maxlat'
 write(9,'(a,4f9.3)') '     ',x(1),x(2),y(1),y(2)
 write(9,*) 'ProfileB:  minlon   maxlon   minlat  maxlat'
 write(9,'(a,4f9.3)') '     ',x(3),x(4),y(3),y(4)
 deallocate(sta,slat,slon,dist,az,elat,elon,depth,mag,x,y)
 61 continue


 1000 continue
 close(9)
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
c get number of values, separated by spaces, in a given line
 implicit none
 character::line*220
 integer*4::nv,j,k,trimlen
 j=0
 do k=2,trimlen(line)
  if(line(k:k).ne.' '.and.line(k-1:k-1).eq.' ') j=j+1
 enddo
 if(line(1:1).ne.' ') j=j+1
 nv=j
end subroutine


subroutine indexx(n,arrin,indx)
!从小到大排序,只记录位置信息indx(n),不交换数组arrin
!从小到大显示数组arrin:
!do i=1,n
! write(*,*) arrin(indx(i))
!enddo
 implicit none
 integer*4::i,j,l,ir,n,indxt,indx(n)
 real*4::q,arrin(n)
 if (n.lt.1) then
  return
 elseif (n.eq.1) then
  indx(1) = 1
  return
 endif
 do j=1,n
  indx(j)=j
 enddo
 l=n/2+1
 ir=n
 10 continue
 if(l.gt.1)then
  l=l-1
  indxt=indx(l)
  q=arrin(indxt)
 else
  indxt=indx(ir)
  q=arrin(indxt)
  indx(ir)=indx(1)
  ir=ir-1
  if(ir.eq.1)then
   indx(1)=indxt
   return
  endif
 endif
 i=l
 j=l+l
 20 if(j.le.ir)then
     if(j.lt.ir)then
      if(arrin(indx(j)).lt.arrin(indx(j+1)))j=j+1
     endif
     if(q.lt.arrin(indx(j)))then
      indx(i)=indx(j)
      i=j
      j=j+j
     else
      j=ir+1
     endif
     goto 20
    endif
    indx(i)=indxt
    goto 10
end subroutine


subroutine GetDistAz(lat1,long1,lat2,long2,dist,az)
!由两点经纬度计算大圆劣弧长和方位角
!longitude:经度;latitude:纬度
!东经为正,西经为负,北纬为正,南纬为负
!1为观测点,2为目标点,az为1相对2的方位角;
!1为事件,2为台站, az为事件到台站的方位角;
 implicit none
 real*4::lat1,long1,lat2,long2,dist,az
 real*4::radlat1,radlong1,radlat2,radlong2,a,b,radlat
 real*4,parameter::EARTH_RADIUS=6371.004e0  !平均半径
 !real*4,parameter::EARTH_RADIUS=6378.137e0  !赤道半径
 real*4,parameter::pi=3.1415926535898e0
 radlat1=lat1*pi/180.0e0
 radlong1=long1*pi/180.0e0
 radlat2=lat2*pi/180.0e0
 radlong2=long2*pi/180.0e0
 a=radLat1-radLat2
 b=radlong1-radlong2
 radlat=(radlat1+radlat2)/2
 !dist=acos(sin(radlat1)*sin(radlat2)+cos(radlat1)*cos(radlat2)*cos(b))*EARTH_RADIUS
 dist=2*asin(sqrt(sin(a/2)*sin(a/2)+cos(radlat1)*cos(radlat2)*sin(b/2)*sin(b/2)))*EARTH_RADIUS
 if(lat1.eq.lat2) then
  az=90.0e0
 else
  az=atan(b/a*cos(radlat))*180.0e0/pi
 endif
 if(lat1.gt.lat2) then
  az=az+180.0e0
 endif
 if(az.lt.0.0e0) then
  az=az+360.0e0
 endif
 return
end subroutine


SUBROUTINE LSLF(X,Y,N,K,CC,RMSEY,RMSE1,RMSE2)
!最小二乘直线拟合
!(X(N),Y(N)):数据坐标,K(2):拟合的直线方程的系数,即Y=K(1)+K(2)*X
!CC:拟合数据与原数据的相关系数;RMSEY:拟合数据与原数据的均方根误差(标准差)
!RMSE1:K(1)的标准差;RMSE2:K(2)的标准差;
 IMPLICIT NONE
 INTEGER*4::N,I,J
 REAL*4::X(N),Y(N),K(2)
 REAL*4::A,B,AA,BB,AB,CC,RMSEY,RMSE1,RMSE2
 A=0.0E0
 B=0.0E0
 AA=0.0E0
 BB=0.0E0
 AB=0.0E0
 DO I=1,N
  A=A+X(I)
  B=B+Y(I)
  AA=AA+X(I)*X(I)
  BB=BB+Y(I)*Y(I)
  AB=AB+X(I)*Y(I)
 ENDDO
 K(2)=(AB*N-A*B)/(AA*N-A*A)
 K(1)=B/N-K(2)*A/N
 CC=(AB-A*B/N)/SQRT((AA-A*A/N)*(BB-B*B/N))
 RMSEY=0.0E0
 DO I=1,N
  RMSEY=RMSEY+(Y(I)-K(1)-K(2)*X(I))*(Y(I)-K(1)-K(2)*X(I))
 ENDDO
 RMSEY=SQRT(RMSEY/(N-2))
 RMSE1=SQRT(AA/(AA*N-A*A))*RMSEY
 RMSE2=RMSEY/SQRT(AA-A*A/N)
 RETURN
END SUBROUTINE


subroutine mean(a,n,av)
!求数组的平均值
 implicit none
 integer*4::i,n
 real*4::a(n),av
 av=0.0e0
 do i=1,n
  av=av+a(i)
 enddo
 av=av/n
 return
end subroutine





