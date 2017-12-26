!本文件为子程序和子函数为文件，保存和便于以后使用。
!作者: 王清东    时间: 2011-05-02 14:34:24
program mysubs
end program


subroutine exist(fn)
 implicit none
 character*(*)::fn
 logical::ex
 inquire(file=fn,exist=ex)
 if(.not.ex) then
  write(*,'("file does not exist ",a)') trim(fn)
  stop
 endif
 return
end subroutine


subroutine freeunit(iunit)
!find a free fortran i/o unit-number
 implicit none
 integer*4::iunit
 logical::lopen
 do iunit=10,999
  if(iunit.eq.999) stop'freeunit>>> no free unit found!'
  inquire(unit=iunit,opened=lopen)
  if(.not.lopen) return
  enddo
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





subroutine isnul(string)
 !字符串中nul变为空格
 implicit none
 integer*4::i,strlen
 character*(*)::string
 strlen=len_trim(string)
 do i=1,strlen
  if(ichar(string(i:i))==0) then
   string(i:i)=' '  !ichar('NUL')=0  ichar(' ')=32
  endif
 enddo
 return
end subroutine


subroutine lower(string)
 !字符串大写变小写
 implicit none
 integer*4::i,strlen
 character*(*)::string
 strlen=len_trim(string)
 do i=1,strlen
  if(string(i:i)>='A'.and.string(i:i)<='Z') then
   string(i:i)=char(ichar(string(i:i))+ichar('a')-ichar('A'))
  endif
 enddo
 return
end subroutine


subroutine upper(string)
 !字符串小写变大写
 implicit none
 integer*4::i,strlen
 character*(*)::string
 strlen=len_trim(string)
 do i=1,strlen
  if(string(i:i)>='a'.and.string(i:i)<='z') then
   string(i:i)=char(ichar(string(i:i))-ichar('a')+ichar('A'))
  endif
 enddo
 return
end subroutine


subroutine npow2(n,nfft,logn)
 implicit none
 integer*4::n,nfft,logn
 nfft=1
 logn=0
 do while(nfft.lt.n)
  nfft=2*nfft
  logn=logn+1
 enddo
 return
end subroutine


subroutine sorti(n,ia)
!从小到大排序,sort an int array
 implicit none
 integer*4::i,j,l,n,ir,iia,ia(n)
 if (n.le.1) then
  return
 endif
 l=n/2+1
 ir=n
 10 continue
 if(l.gt.1)then
  l=l-1
  iia=ia(l)
 else
  iia=ia(ir)
  ia(ir)=ia(1)
  ir=ir-1
  if(ir.eq.1)then
   ia(1)=iia
   return
  endif
 endif
 i=l
 j=l+l
 20 if(j.le.ir)then
     if(j.lt.ir)then
      if(ia(j).lt.ia(j+1))j=j+1
     endif
     if(iia.lt.ia(j))then
      ia(i)=ia(j)
      i=j
      j=j+j
     else
      j=ir+1
     endif
     goto 20
    endif
    ia(i)=iia
    goto 10
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


integer*4 function ifindi(n,ia,iv)
!在有序的整数向量(由小到大)中寻找指定值的位置
 implicit none
 integer*4::i,n,k ! 2^(no. of chops)
 integer*4::ia(n)	! [1..n] Vector to search
 integer*4::iv	  ! Value to find
 if(n.le.0) then
  ifindi = 0
  return
 endif
 if(iv.lt.ia(1).or.iv.gt.ia(n)) then
!Outside range of vector
  ifindi=0
  return
 endif
 k=2
 i=nint(real(n)/k)
 10 if(k.gt.2*n) then
!Value not in vector
  ifindi=0
  return
 endif
 k=k*2
 if(iv.lt.ia(i)) then
!Value smaller:  Search below
  i=i-nint(real(n)/k)
  goto 10
 endif
 if(iv.gt.ia(i))then
!Value larger:  Search above
  i=i+nint(real(n)/k)
  goto 10
 endif
!Value found: iv == ia[i]
 ifindi=i
 return
end subroutine


subroutine smooth(data,n)
!实验数据的等距五点三次平滑
 implicit none
 integer*4::i,n
 real*4::a(n),data(n)
 a(1)=(69*data(1)+4*data(2)-6*data(3)+4*data(4)-data(5))/70
 a(2)=(2*data(1)+27*data(2)+12*data(3)-8*data(4)+2*data(5))/35
 do i=3,n-2
  a(i)=(-3*data(i-2)+12*data(i-1)+17*data(i)+12*data(i+1)-3*data(i+2))/35
 enddo
 a(n-1)=(2*data(n-4)-8*data(n-3)+12*data(n-2)+27*data(n-1)+2*data(n))/35
 a(n)=(-data(n-4)+4*data(n-3)-6*data(n-2)+4*data(n-1)+69*data(n))/70
 data=a
 return
end subroutine


subroutine dot(str,n,i)
!寻找字符串str中的第n个.号的位置i
 implicit none
 integer*4::i,j,k,n
 character*1::flag
 character*(*)::str
 if(n==0) then
  i=0
  return
 endif
 j=len_trim(str)
 k=0
 do i=1,j
  flag=str(i:i)
  if(flag=='.') then
   k=k+1
   if(k==n) exit
  endif
 enddo
 if(k==0) print*, 'No dot in:',str
 return
end subroutine


subroutine date(year,month,day,days)
!将日期转化为一年中的第几天;即SAC中的NZJDAY
 implicit none
 integer*4::i,year,month,day,days
 integer*4::mon(12)
 data mon /31,28,31,30,31,30,31,31,30,31,30,31/
 mon(2)=28
 !判断是否闰年:能被4整除但不能被100整除以及能被400整除的年份为闰年。
 if(mod(year,4)==0.and.mod(year,100)/=0) then
  mon(2)=29
 elseif(mod(year,400)==0) then
  mon(2)=29
 endif
 days=day
 do i=1,month-1
  days=days+mon(i)
 enddo
 return
end subroutine


subroutine andate(year,days,month,day)
!将一年中的第几天(SAC中的NZJDAY)转化为年月日
 implicit none
 integer*4::i,year,month,day,days
 integer*4::mon(12)
 data mon /31,28,31,30,31,30,31,31,30,31,30,31/
 mon(2)=28
 !判断是否闰年:能被4整除但不能被100整除以及能被400整除的年份为闰年。
 if(mod(year,4)==0.and.mod(year,100)/=0) then
  mon(2)=29
 elseif(mod(year,400)==0) then
  mon(2)=29
 endif
 day=days
 do i=1,12
  if(day<mon(i)) then
   month=i
   exit
  endif
  day=day-mon(i)
 enddo
 return
end subroutine


subroutine nRunNian(yearS,yearE,nRN)
!计算yearS至yearE间有几个闰年，包括yearS和yearE
 implicit none
 integer*4::yearS,yearE,nyear,nRN
 integer*4::i,bet(3),n(3)
 nyear=yearE-yearS
 bet(1)=4
 bet(2)=100
 bet(3)=400
 do i=1,3
  if(mod(nyear,bet(i))==0) then
   n(i)=nyear/bet(i)
  else
   if(mod(nyear,bet(i))>=mod(yearE,bet(i))) then
    n(i)=nyear/bet(i)+1
   else
    n(i)=nyear/bet(i)
   endif
  endif
 enddo
 nRN=n(1)-n(2)+n(3)
 return
end subroutine


subroutine ndays(yearS,monthS,dayS,yearE,monthE,dayE,nday)
!计算日期yearS-monthS-dayS至yearE-monthE-dayE间的天数
 implicit none
 integer*4::yearS,monthS,dayS,yearE,monthE,dayE,nday
 integer*4::nyear,nRN,day1,day2,day3
 integer*4::i,bet(3),n(3),mon(12)
 data mon /31,28,31,30,31,30,31,31,30,31,30,31/
 mon(2)=28
 !判断是否闰年:能被4整除但不能被100整除以及能被400整除的年份为闰年。
 if(mod(yearS,4)==0.and.mod(yearS,100)/=0) then
  mon(2)=29
 elseif(mod(yearS,400)==0) then
  mon(2)=29
 endif
 day1=dayS
 do i=1,monthS-1
  day1=day1+mon(i)
 enddo
 mon(2)=28
 if(mod(yearE,4)==0.and.mod(yearE,100)/=0) then
  mon(2)=29
 elseif(mod(yearE,400)==0) then
  mon(2)=29
 endif
 day3=dayE
 do i=1,monthE-1
  day3=day3+mon(i)
 enddo
 nyear=yearE-1-yearS
 bet(1)=4
 bet(2)=100
 bet(3)=400
 do i=1,3
  if(mod(nyear,bet(i))==0) then
   n(i)=nyear/bet(i)
  else
   if(mod(nyear,bet(i))>=mod(yearE-1,bet(i))) then
    n(i)=nyear/bet(i)+1
   else
    n(i)=nyear/bet(i)
   endif
  endif
 enddo
 nRN=n(1)-n(2)+n(3)
 day2=(nyear+1)*365+nRN
 nday=day2-day1+day3
 return
end subroutine


subroutine ndays2(yearS,daysS,yearE,daysE,nday)
!计算日期yearS-daysS至yearE-daysE间的天数
 implicit none
 integer*4::yearS,daysS,yearE,daysE,nday
 integer*4::nyear,nRN,day1,day2,day3
 integer*4::i,bet(3),n(3)!,mon(12)
 !data mon /31,28,31,30,31,30,31,31,30,31,30,31/
 !判断是否闰年:能被4整除但不能被100整除以及能被400整除的年份为闰年。
 day1=daysS
 day3=daysE
 nyear=yearE-1-yearS
 bet(1)=4
 bet(2)=100
 bet(3)=400
 do i=1,3
  if(mod(nyear,bet(i))==0) then
   n(i)=nyear/bet(i)
  else
   if(mod(nyear,bet(i))>=mod(yearE-1,bet(i))) then
    n(i)=nyear/bet(i)+1
   else
    n(i)=nyear/bet(i)
   endif
  endif
 enddo
 nRN=n(1)-n(2)+n(3)
 day2=(nyear+1)*365+nRN
 nday=day2-day1+day3
 return
end subroutine


subroutine nor(data,n)
 implicit none
 integer*4::n,i
 real*4::data(n),max
 max=0.0e0
 do i=1,n
  if(max.lt.abs(data(i))) max=abs(data(i))
 enddo
 data=data/max
 return
end subroutine


subroutine nord(data,n,B,DT)
!双端信号两端单独归一化
 implicit none
 integer*4::n,i,zero
 real*4::data(n),max1,max2,B,DT
 zero=anint(-B/DT)+1
 max1=0.0e0
 max2=0.0e0
 do i=1,zero-1
  if(max1.lt.abs(data(i))) max1=abs(data(i))
 enddo
 data(1:zero-1)=data(1:zero-1)/max1
 do i=zero+1,n
  if(max2.lt.abs(data(i))) max2=abs(data(i))
 enddo
 data(zero+1:n)=data(zero+1:n)/max2
 data(zero)=data(zero)/max(max1,max2)
 return
end subroutine


integer*4 function trimlen(t)
!Length of character string, excluding trailing blanks
 implicit none
 character*(*)::t
 do trimlen=len(t),1,-1
  if(t(trimlen:trimlen).ne.' ') return
 enddo
 trimlen=1
end function


subroutine Gauss(data,n,dt,tc,alpha)
! 截断为pai=3.14159265358e0
 implicit none
 integer*4::i,n,logn,nfft
 real*4::data(n),dt,tc,fc,df,f,alpha,fac,freqlw,frequp,G
 real*4,parameter::pai=3.14159265358e0
 real*4,allocatable::PR(:),PI(:),FR(:),FI(:)
 logn=floor(log10(real(n))/log10(2.0e0))+1
 nfft=2**logn
 !call npow2(n,nfft,logn)
 fc=1.0e0/tc
 df=1.0e0/(nfft*dt)
 !print*, 'logn=',logn,'nfft=',nfft
 allocate(PR(nfft))
 allocate(PI(nfft))
 allocate(FR(nfft))
 allocate(FI(nfft))
 PR=0.0e0
 PI=0.0e0
 FR=0.0e0
 FI=0.0e0
 PR(1:n)=data(1:n)
 call KKFFT(PR,PI,nfft,logn,FR,FI,0,0)
 fac=sqrt(pai/alpha)
 frequp=(1.0e0+fac)*fc
 freqlw=(1.0e0-fac)*fc
 do i=1,nfft
  f=(i-1)*df
  if(f.ge.freqlw.and.f.le.frequp) then
   G=exp(-alpha*(f/fc-1.0e0)**2)
  else
   G=0.0e0
  endif
  PR(i)=FR(i)*G
  PI(i)=FI(i)*G
 enddo
 PR(nfft/2+1)=0.5e0*PR(nfft/2+1)
 PI(nfft/2+1)=0.5e0*PI(nfft/2+1)
 PR(nfft/2+2:nfft)=0.0e0
 PI(nfft/2+2:nfft)=0.0e0       !令负频率为零
 call KKFFT(PR,PI,nfft,logn,FR,FI,1,0)
 data(1:n)=2*FR(1:n)
 deallocate(PR)
 deallocate(PI)
 deallocate(FR)
 deallocate(FI)
 return
end subroutine


subroutine taper(data,n,width)
!仿照sac2000中的taper命令
!DATA(J)=DATA(J)*(F0-F1*COS(OMEGA*(J-1))
!======== ========= ===== ======
!TYPE     OMEGA     F0    F1
!======== ========= ===== ======
!HANNING   PI/N     0.50  0.50
!HAMMING   PI/N     0.54  0.46
!COSINE    PI/(2*N) 1.00  1.00
!======== ========= ===== ======
!这里选用HANNING
 implicit none
 integer*4::i,j,k,n,tl,tr
 real*4::data(n),width,f0,f1,omega
 real*4,parameter::pi=3.14159265358e0
 do i=1,n
  if(data(i).ne.0.0e0) exit
 enddo
 if(i.eq.n) then
  print*, 'This is a zero data, please check'
  return
 endif
 tl=i
 do i=n,1,-1
  if(data(i).ne.0.0e0) exit
 enddo
 tr=i
 f0=0.5e0
 f1=0.5e0
 j=anint((tr-tl+1)*width)
 if(j.eq.1.or.j.eq.0) return
 omega=pi/j
 do i=tl,tl+j-1
  data(i)=data(i)*(f0-f1*cos(omega*(i-tl)))
 enddo
 do i=tr,tr-j+1,-1
  data(i)=data(i)*(f0-f1*cos(omega*(i-tr)))
 enddo
 return
end subroutine


subroutine taper2(data,n,nn)
!仿照sac2000中的taper命令
!DATA(J)=DATA(J)*(F0-F1*COS(OMEGA*(J-1))
!======== ========= ===== ======
!TYPE     OMEGA     F0    F1
!======== ========= ===== ======
!HANNING   PI/N     0.50  0.50
!HAMMING   PI/N     0.54  0.46
!COSINE    PI/(2*N) 1.00  1.00
!======== ========= ===== ======
!这里选用HANNING
 implicit none
 integer*4::i,n,nn,tl,tr
 real*4::data(n),f0,f1,omega
 real*4,parameter::pi=3.14159265358e0
 f0=0.5e0
 f1=0.5e0
 omega=pi/nn
 do i=1,n
  if(data(i).ne.0.0e0) exit
 enddo
 if(i.eq.n) return
 tl=i
 do i=n,1,-1
  if(data(i).ne.0.0e0) exit
 enddo
 tr=i
 if(tl+nn-1.gt.n.or.tr-nn+1.lt.1) then
  print*, 'The nn is too large, please check!'
  return
 endif
 do i=tl,tl+nn-1
  data(i)=data(i)*(f0-f1*cos(omega*(i-tl)))
 enddo
 do i=tr,tr-nn+1,-1
  data(i)=data(i)*(f0-f1*cos(omega*(i-tr)))
 enddo
 return
end subroutine


subroutine nbf(data,n,dt,tc,hf,minhf)
! tc:滤波中心周期
! hf:频率窗半宽度
 implicit none
 integer*4::i,n,logn,nfft
 real*4::data(n),dt,df,tc,hf,fc,f1,f2,f,nb,minhf
 real*4,parameter::pai=3.14159265358e0
 real*4,allocatable::PR(:),PI(:),FR(:),FI(:)
 !logn=floor(log10(real(n))/log10(2.0e0))+1
 !nfft=2**logn
 call npow2(n,nfft,logn)
 !print*, 'logn=',logn,'nfft=',nfft
 fc=1.0e0/tc
 df=1.0e0/(nfft*dt)
 do while(df.gt.minhf)
  nfft=2*nfft
  logn=logn+1
  df=1.0e0/(nfft*dt)
 enddo
 f1=fc-hf
 f2=fc+hf
 allocate(PR(nfft),PI(nfft),FR(nfft),FI(nfft))
 PR=0.0e0
 PI=0.0e0
 FR=0.0e0
 FI=0.0e0
 call taper(data,n,0.05)
 PR(1:n)=data(1:n)
 call KKFFT(PR,PI,nfft,logn,FR,FI,0,0)
 !print*, b,npts,dt,t0,n,m
 do i=1,nfft/2+1
  f=(i-1)*df
  if(f.ge.f1.and.f.le.f2) then
   nb=(cos(pai*(f-fc)/hf)+1.0e0)/2
  else
   nb=0.0e0
  endif
  PR(i)=FR(i)*nb
  PI(i)=FI(i)*nb
 enddo
 PR(1)=PR(1)/2
 PI(1)=PI(1)/2
 PR(nfft/2+1)=PR(nfft/2+1)/2
 PI(nfft/2+1)=PI(nfft/2+1)/2
 PR(nfft/2+2:nfft)=0.0e0
 PI(nfft/2+2:nfft)=0.0e0       !令负频率为零
 call taper(PR,nfft,0.05)
 call taper(PI,nfft,0.05)
 call KKFFT(PR,PI,nfft,logn,FR,FI,1,0)
 data(1:n)=2*FR(1:n)
 deallocate(PR,PI,FR,FI)
 return
end subroutine


subroutine bpf(data,nfft,logn,dt,t1,t2,t3,t4)
 implicit none
 integer::i,logn,nfft
 real*4::dt,df,t1,t2,t3,t4,f1,f2,f3,f4,f,bp
 real*4::data(nfft),PR(nfft),PI(nfft),FR(nfft),FI(nfft)
 real*4,parameter::pai=3.14159265358e0
 df=1.0e0/(nfft*dt)
 f1=1.0e0/t4
 f2=1.0e0/t3
 f3=1.0e0/t2
 f4=1.0e0/t1
 !print*, df,f1,f2,f3,f4
 PR=0.0e0
 PI=0.0e0
 FR=0.0e0
 FI=0.0e0
 PR=data
 call taper(PR,nfft,0.005)                !平滑数据的两端,避免吉普斯现象
 call KKFFT(PR,PI,nfft,logn,FR,FI,0,0)
 do i=1,nfft/2+1
  f=(i-1)*df
  if(f.ge.f1.and.f.le.f2) then
   bp=0.5e0*(cos(pai*(f-f2)/(f2-f1))+1.0e0)
  elseif(f.gt.f2.and.f.lt.f3) then
   bp=1.0e0
  elseif(f.ge.f3.and.f.le.f4) then
   bp=0.5e0*(cos(pai*(f-f3)/(f4-f3))+1.0e0)
  else
   bp=0.0e0
  endif
  PR(i)=FR(i)*bp
  PI(i)=FI(i)*bp
 enddo
 PR(1)=PR(1)/2
 PI(1)=PI(1)/2
 PR(nfft/2+1)=PR(nfft/2+1)/2
 PI(nfft/2+1)=PI(nfft/2+1)/2
 PR(nfft/2+2:nfft)=0.0e0
 PI(nfft/2+2:nfft)=0.0e0       !令负频率为零
 call taper(PR,nfft,0.005)
 call taper(PI,nfft,0.005)
 call KKFFT(PR,PI,nfft,logn,FR,FI,1,0)
 data=2*FR
 return
end subroutine


subroutine bpfn(data,n,dt,t1,t2,t3,t4)
 implicit none
 integer::i,n,logn,nfft
 real*4::dt,df,t1,t2,t3,t4,f1,f2,f3,f4,f,bp
 real*4::data(n)
 real*4,allocatable::PR(:),PI(:),FR(:),FI(:)
 real*4,parameter::pai=3.14159265358e0
 call npow2(n,nfft,logn)
 allocate(PR(nfft),PI(nfft),FR(nfft),FI(nfft))
 df=1.0e0/(nfft*dt)
 f1=1.0e0/t4
 f2=1.0e0/t3
 f3=1.0e0/t2
 f4=1.0e0/t1
 !print*, df,f1,f2,f3,f4
 PR=0.0e0
 PI=0.0e0
 FR=0.0e0
 FI=0.0e0
 PR(1:n)=data(1:n)
 call taper(PR,nfft,0.25)                !平滑数据的两端,避免吉普斯现象
 call KKFFT(PR,PI,nfft,logn,FR,FI,0,0)
 do i=1,nfft/2+1
  f=(i-1)*df
  if(f.ge.f1.and.f.le.f2) then
   bp=0.5e0*(cos(pai*(f-f2)/(f2-f1))+1.0e0)
  elseif(f.gt.f2.and.f.lt.f3) then
   bp=1.0e0
  elseif(f.ge.f3.and.f.le.f4) then
   bp=0.5e0*(cos(pai*(f-f3)/(f4-f3))+1.0e0)
  else
   bp=0.0e0
  endif
  PR(i)=FR(i)*bp
  PI(i)=FI(i)*bp
 enddo
 PR(1)=PR(1)/2
 PI(1)=PI(1)/2
 PR(nfft/2+1)=PR(nfft/2+1)/2
 PI(nfft/2+1)=PI(nfft/2+1)/2
 PR(nfft/2+2:nfft)=0.0e0
 PI(nfft/2+2:nfft)=0.0e0       !令负频率为零
 !call taper(PR,nfft,0.005)
 !call taper(PI,nfft,0.005)
 call KKFFT(PR,PI,nfft,logn,FR,FI,1,0)
 data(1:n)=2*FR(1:n)
 return
end subroutine


subroutine corcoe(x,y,n,cc)
!本程序计算x(n),y(n)两列向量的相关系数cc
 implicit none
 integer*4::n,i
 real*4::x(n),y(n),cc,a,b,aa,bb,ab
 a=0.0e0
 b=0.0e0
 aa=0.0e0
 bb=0.0e0
 ab=0.0e0
 do i=1,n
  a=a+x(i)
  b=b+y(i)
  aa=aa+x(i)*x(i)
  bb=bb+y(i)*y(i)
  ab=ab+x(i)*y(i)
 enddo
 cc=(ab-a*b/n)/sqrt((aa-a*a/n)*(bb-b*b/n))
 return
end subroutine


SUBROUTINE TCOR(X,M,H,N,Y,LN,RN)
 IMPLICIT NONE
! This program is purposed to calculate the auto-correlation or
! corss-correlation function in time-domain. Creatied: 2010/12/17 15:25:44
 INTEGER*4::I,J,M,N,LN,RN
 REAL*4::X(M),H(N),Y(LN:RN)
! X(M),H(N)为输入，Y(-M+1:N-1)为输出；Y为X与H的相关序列，共M+N-1项
! Y(0)  为X(M),H(N)首项对齐时的相关值；
! Y(1)  为H(N)相对X(M)左移一项时的相关值，正向分支共N-1项；
! Y(-1) 为H(N)相对X(M)右移一项时的相关值，负向分支共M-1项。
! LN表示所求的相关序列的左边界，RN表示所求的相关序列的右边界。
 IF(LN.LT.-M+1) PRINT*, '左边界超出'
 IF(RN.GT.N-1) PRINT*, '右边界超出'
 DO I=LN,RN
  Y(I)=0.0e0
  DO J=1,M
   IF(J+I.GE.1.AND.J+I.LE.N) Y(I)=Y(I)+X(J)*H(J+I)
  ENDDO
 ENDDO
 RETURN
END SUBROUTINE


SUBROUTINE FCOR(X,M,H,N,Y)
 IMPLICIT NONE
! THIS PROGRAM IS PURPOSED TO CALCULATE THE AUTO-CORRELATION OR
! CORSS-CORRELATION FUNCTION IN FREQUENCE-DOMAIN. CREATIED: 2011-01-04 20:20:27
 INTEGER*4::I,M,N,LOGN,NFFT
 REAL*4::X(M),H(N),Y(M+N-1)
 REAL*4,ALLOCATABLE::PR(:),PI(:),FR(:),FI(:),HFR(:),HFI(:)
! X(M),H(N)为输入，Y(M+N-1)为输出；Y为X与H的相关序列，共M+N-1项
 !LOGN=FLOOR(LOG10(M+N-1.0E0)/LOG10(2.0E0))+1
 !NFFT=2**LOGN
 CALL NPOW2(M+N-1,NFFT,LOGN)
 !PRINT*, LOGN,NFFT
 ALLOCATE(PR(NFFT),PI(NFFT),FR(NFFT),FI(NFFT),HFR(NFFT),HFI(NFFT))
 PR=0.0E0
 PI=0.0E0
 FR=0.0E0
 FI=0.0E0
 HFR=0.0E0
 HFI=0.0E0
 PR(1:M)=X(1:M)
 CALL TAPER(PR,NFFT,0.005)
 CALL KKFFT(PR,PI,NFFT,LOGN,FR,FI,0,0)
 PR=0.0E0
 PI=0.0E0
 PR(1:N)=H(1:N)
 CALL TAPER(PR,NFFT,0.005)
 CALL KKFFT(PR,PI,NFFT,LOGN,HFR,HFI,0,0)
 DO I=1,NFFT
  PR(I)=FR(I)*HFR(I)+FI(I)*HFI(I)
  PI(I)=FR(I)*HFI(I)-FI(I)*HFR(I)
 ENDDO
 CALL TAPER(PR,NFFT,0.005)
 CALL TAPER(PI,NFFT,0.005)
 CALL KKFFT(PR,PI,NFFT,LOGN,FR,FI,1,0)
! OUT(1:NLEN1-1)=YTMP(NFFT-NLEN1+2:NFFT)
! OUT(NLEN1:NLEN1+NLEN2-1)=YTMP(1:NLEN2)
 DO I=1,M-1
  Y(I)=FR(NFFT-M+I+1)
 ENDDO
 DO I=1,N
  Y(M+I-1)=FR(I)
 ENDDO
 DEALLOCATE(PR,PI,FR,FI,HFR,HFI)
 RETURN
END SUBROUTINE


SUBROUTINE SCOR(X,M,H,N,Y)
 IMPLICIT NONE
! THIS PROGRAM IS PURPOSED TO CALCULATE THE AUTO-CORRELATION OR
! CORSS-CORRELATION FUNCTION IN FREQUENCE-DOMAIN(FORM SAC2000). CREATIED: 2011-01-04 20:20:31
 INTEGER*4::I,J,M,N,LOGN,NFFT
 REAL*4::X(M),H(N),Y(M+N-1),XR,XI,YR,YI
 REAL*4,ALLOCATABLE::PR(:),PI(:),FR(:),FI(:)
! X(M),H(N)为输入，Y(M+N-1)为输出；Y为X与H的相关序列，共M+N-1项
 !LOGN=FLOOR(LOG10(M+N-1.0E0)/LOG10(2.0E0))+1
 !NFFT=2**LOGN
 CALL NPOW2(M+N-1,NFFT,LOGN)
 !PRINT*, LOGN,NFFT
 ALLOCATE(PR(NFFT),PI(NFFT),FR(NFFT),FI(NFFT))
 PR=0.0E0
 PI=0.0E0
 FR=0.0E0
 FI=0.0E0
 PR(1:M)=X(1:M)
 PI(1:N)=H(1:N)
 CALL TAPER(PR,NFFT,0.005)
 CALL TAPER(PI,NFFT,0.005)
 CALL KKFFT(PR,PI,NFFT,LOGN,FR,FI,0,0)
 PR=0.0E0
 PI=0.0E0
 PR(1)=FR(1)*FI(1)
 DO I=2,NFFT/2+1
  J=NFFT-I+2
  XR=(FR(I)+FR(J))/2
	XI=(FI(I)-FI(J))/2
	YR=(FI(I)+FI(J))/2
	YI=(FR(J)-FR(I))/2
	PR(I)=XR*YR+XI*YI
  PI(I)=XR*YI-XI*YR
	PR(J)=PR(I)
	PI(J)=-PI(I)
 ENDDO
 CALL TAPER(PR,NFFT,0.005)
 CALL TAPER(PI,NFFT,0.005)
 CALL KKFFT(PR,PI,NFFT,LOGN,FR,FI,1,0)
! Y(1:NLEN1-1)=FR(NFFT-NLEN1+2:NFFT)
! Y(NLEN1:NLEN1+NLEN2-1)=FR(1:NLEN2)
 DO I=1,M-1
  Y(I)=FR(NFFT-M+I+1)
 ENDDO
 DO I=1,N
  Y(M+I-1)=FR(I)
 ENDDO
 DEALLOCATE(PR,PI,FR,FI)
 RETURN
END SUBROUTINE


SUBROUTINE SCORF(A,NA,B,NB,CR,CI,NFFT,LOGN)
 IMPLICIT NONE
! THIS PROGRAM IS PURPOSED TO CALCULATE THE AUTO-CORRELATION OR
! CORSS-CORRELATION FUNCTION IN FREQUENCE-DOMAIN(FORM SAC2000). CREATIED: 2011-01-04 20:20:31
 INTEGER*4::I,J,NA,NB,LOGN,NFFT
 REAL*4::A(NA),B(NB),CR(NFFT),CI(NFFT),PR(NFFT),PI(NFFT),FR(NFFT),FI(NFFT),XR,XI,YR,YI
 PR=0.0E0
 PI=0.0E0
 FR=0.0E0
 FI=0.0E0
 CALL TAPER(A,NA,0.005)
 CALL TAPER(B,NB,0.005)
 PR(1:NA)=A(1:NA)
 PI(1:NB)=B(1:NB)
 CALL KKFFT(PR,PI,NFFT,LOGN,FR,FI,0,0)
 PR=0.0E0
 PI=0.0E0
 PR(1)=FR(1)*FI(1)
 DO I=2,NFFT/2+1
  J=NFFT-I+2
  XR=(FR(I)+FR(J))/2
	XI=(FI(I)-FI(J))/2
	YR=(FI(I)+FI(J))/2
	YI=(FR(J)-FR(I))/2
	PR(I)=XR*YR+XI*YI
  PI(I)=XR*YI-XI*YR
	PR(J)=PR(I)
	PI(J)=-PI(I)
 ENDDO
 CR=PR
 CI=PI
 RETURN
END SUBROUTINE


subroutine ICOR(PR,PI,nfft,logn,dataL,dataR,m,n)
 integer*4::nfft,logn,m,n,i
 real*4::PR(nfft),PI(nfft),FR(nfft),FI(nfft),dataL(m),dataR(n),dataC(m+n-1)
 call taper(PR,nfft,0.5)
 call taper(PI,nfft,0.5)
 call KKFFT(PR,PI,nfft,logn,FR,FI,1,0)
 !Y(1:NLEN1-1)=FR(NFFT-NLEN1+2:NFFT)
 !Y(NLEN1:NLEN1+NLEN2-1)=FR(1:NLEN2)
 do i=1,m-1
  dataC(i)=FR(nfft-m+i+1)
 enddo
 do i=1,n
  dataC(m+i-1)=FR(i)
 enddo
 do i=1,m
  dataL(i)=dataC(m+1-i)
 enddo
 do i=1,n
  dataR(i)=dataC(m+i-1)
 enddo
end subroutine


SUBROUTINE myFFT(data,NPTS,N,K,FR,FI)
!方便循环使用
 IMPLICIT NONE
 INTEGER*4::NPTS,N,K
 REAL*4::data(NPTS),PR(N),PI(N),FR(N),FI(N)
 PR=0.0e0
 PI=0.0e0
 PR(1:NPTS)=data(1:NPTS)
 call taper(PR,N,0.5)
 call KKFFT(PR,PI,N,K,FR,FI,0,0)
 RETURN
END SUBROUTINE


SUBROUTINE KKFFT(PR,PI,N,K,FR,FI,L,IL)
!L=0 for fft;L=1 for ifft
!IL=0 不计算模与幅角;IL=1 计算模与幅角
 IMPLICIT NONE
 INTEGER*4::I,N,K,IT,L,IL,M,IS,J,NV,L0
 REAL*4::PR(N),PI(N),FR(N),FI(N),P,Q,S,VR,VI,PODDR,PODDI
 DO IT=0,N-1
  M=IT
	IS=0
	DO I=0,K-1
	 J=M/2
	 IS=2*IS+(M-2*J)
	 M=J
  ENDDO
	FR(IT+1)=PR(IS+1)
	FI(IT+1)=PI(IS+1)
 ENDDO
 PR(1)=1.0E0
 PI(1)=0.0E0
 PR(2)=COS(6.283185306E0/N)
 PI(2)=-SIN(6.283185306E0/N)
 IF(L.NE.0) PI(2)=-PI(2)
 DO I=3,N
	P=PR(I-1)*PR(2)
	Q=PI(I-1)*PI(2)
	S=(PR(I-1)+PI(I-1))*(PR(2)+PI(2))
	PR(I)=P-Q
	PI(I)=S-P-Q
 ENDDO
 DO IT=0,N-2,2
	VR=FR(IT+1)
	VI=FI(IT+1)
	FR(IT+1)=VR+FR(IT+2)
	FI(IT+1)=VI+FI(IT+2)
	FR(IT+2)=VR-FR(IT+2)
	FI(IT+2)=VI-FI(IT+2)
 ENDDO
 M=N/2
 NV=2
 DO L0=K-2,0,-1
	M=M/2
	NV=2*NV
	DO IT=0,(M-1)*NV,NV
	 DO J=0,(NV/2)-1
	  P=PR(M*J+1)*FR(IT+J+1+NV/2)
	  Q=PI(M*J+1)*FI(IT+J+1+NV/2)
	  S=PR(M*J+1)+PI(M*J+1)
	  S=S*(FR(IT+J+1+NV/2)+FI(IT+J+1+NV/2))
	  PODDR=P-Q
	  PODDI=S-P-Q
	  FR(IT+J+1+NV/2)=FR(IT+J+1)-PODDR
    FI(IT+J+1+NV/2)=FI(IT+J+1)-PODDI
	  FR(IT+J+1)=FR(IT+J+1)+PODDR
	  FI(IT+J+1)=FI(IT+J+1)+PODDI
   ENDDO
  ENDDO
 ENDDO
 IF(L.NE.0) THEN
  DO I=1,N
	 FR(I)=FR(I)/N
	 FI(I)=FI(I)/N
  ENDDO
 ENDIF
 IF(IL.NE.0) THEN
  DO I=1,N
	 PR(I)=SQRT(FR(I)*FR(I)+FI(I)*FI(I))
   PI(I)=ATAN2(FI(I),FR(I))*360.0E0/6.283185306E0
   !将相位还原为[0,2pai],即[0,360]
   IF(PI(I).LT.0.0E0) PI(I)=360.0E0+PI(I)
  ENDDO
 ENDIF
 RETURN
END SUBROUTINE


subroutine fork(lx,cx,signi)
! fast fourier,2/15/69; signi=-1:fft , signi=1:ifft .
!                  lx
! cx(k)=sqrt(1/lx) sum (cx(j)*exp(2*pi*signi*i*(j-1)*(k-1)/lx))
!                  j=1           for k= 1,2,...,(lx=2**integer)
   implicit none
   integer*4::signi,i,j,lx,m,l,istep
   real*4::sc
   complex(4)::cx(lx),carg,cexp,cw,ctemp
   j=1
   sc=sqrt(1.0e0/lx)
   do 30 i=1,lx
   if(i.gt.j) goto 10
   ctemp=cx(j)*sc
   cx(j)=cx(i)*sc
   cx(i)=ctemp
10 m=lx/2
20 if(j.le.m) goto 30
   j=j-m
   m=m/2
   if(m.ge.1) goto 20
30 j=j+m
   l=1
40 istep=2*l
   do 50 m=1,l
   carg=(0.0e0,1.0e0)*(3.14159265358e0*signi*(m-1))/l
   cw=cexp(carg)
   do 50 i=m,lx,istep
   ctemp=cw*cx(i+l)
   cx(i+l)=cx(i)-ctemp
50 cx(i)=cx(i)+ctemp
   l=istep
   if(l.lt.lx) goto 40
   return
end subroutine


subroutine fast(n,x,ind)
!ind=-1:fft ; ind=1:ifft . n必须是2的幂.
    implicit none
    integer*4::i,j,k,m,n,ind,kmax,istep
    complex(4)::x(n),temp,theta
	  j=1
	  do 140 i=1,n
	  if(i.ge.j) goto 110
	  temp=x(j)
	  x(j)=x(i)
	  x(i)=temp
110 m=n/2
120 if(j.le.m) goto 130
	  j=j-m
	  m=m/2
	  if(m.ge.2) goto 120
130 j=j+m
140 continue
	  kmax=1
150 if(kmax.ge.n) goto 180
	  istep=kmax*2
	  do 170 k=1,kmax
	   theta=cmplx(0.0e0,3.14159265358e0)*float(ind*(k-1))/float(kmax)
	   do 160 i=k,n,istep
	    j=i+kmax
	    temp=x(j)*cexp(theta)
	    x(j)=x(i)-temp
	    x(i)=x(i)+temp
160  continue
170 continue
	  kmax=istep
	  goto 150
180 if(ind.eq.-1) goto 200
    do 190 i=1,n
     x(i)=x(i)/float(n)
190 continue
200 return
end subroutine


subroutine brsach(IRU,name,nerr)
!-----
!       IRU I*4 logical unit for IO
!       name    C*  Name of file to be opened
!       rhdr    R*4 Real header
!       ihdr    I*4 Integer Header
!       chdr    C*  Character Header
!       nerr    I*4 -1 file does not exist
!               -2 data points in file exceed dimension
!
!       NOTE IF THE BINARY FILE HAS MORE THAN LN POINTS, THEN ONLY
!       LN POINTS ARE USED
!-----
!  This routine reads waveform data written in SAC binary format.
!
!  Written by Hafidh A. A. Ghalib, 1988.
!-----
 implicit none
 integer*4::i,IRU,nerr
 logical::ext
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)
 character*(*)::name
!-----
!  Read real and integer header blocks to find actual number
!  of waveform data points which is stored in ihdr(10).
!-----
 inquire(file=name,exist=ext)
 if(.not.ext) then
  ihdr(10)=0
  nerr=-1
  return
 endif
 nerr=0
 !open(IRU,file=name,form='unformatted',access='direct',recl=440,status='old')
 !read(IRU,rec=1) (rhdr(i),i=1,70),(ihdr(i),i=1,40)
 open(IRU,file=name,form='unformatted',access='direct',recl=632,status='old')
 read(IRU,rec=1) (rhdr(i),i=1,70),(ihdr(i),i=1,40),(chdr(i),i=1,24)
 close(IRU)
 return
end subroutine


subroutine brsac(IRU,LN,name,data,nerr)
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
!  Written by Hafidh A. A. Ghalib, 1988.
!
!  Modified by Wang, 2012
!-----
 implicit none
 integer*4::i,IRU,LN,nerr,maxpts,nbytes
 real*4::data(LN)
 logical::ext
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)
 character*(*)::name
!  Read real and integer header blocks to find actual number
!  of waveform data points which is stored in ihdr(10).
! print*, name
 inquire(file=name,exist=ext)
 if(.not.ext) then
  ihdr(10)=0
  nerr=-1
  return
 endif
 nerr=0
 open(IRU,file=name,form='unformatted',access='direct',recl=440,status='old')
 read(IRU,rec=1) (rhdr(i),i=1,70),(ihdr(i),i=1,40)
 close(IRU)
!  Read header and waveform data blocks using recored length of 158*4=632.
 if(ihdr(10).gt.LN) then
  maxpts=LN
  nerr=-2
 else
  maxpts=ihdr(10)
  nerr=0
 endif
 nbytes=632+4*maxpts
 open(IRU,file=name,form='unformatted',access='direct',recl=nbytes)
 read(IRU,rec=1)  (rhdr(i),i=1,70),   &
                  (ihdr(i),i=1,40),   &
                  (chdr(i),i=1,24),   &
                  (data(i),i=1,maxpts)
 close(IRU)
 ihdr(10)=maxpts
 return
end subroutine


subroutine bwsach(IRU,name,nerr)
!-----
!       IRU I*4 logical unit for IO
!       name    C*  Name of file to be opened
!       rhdr    R*4 Real header
!       ihdr    I*4 Integer Header
!       chdr    C*  Character Header
!       nerr    I*4 -1 file does not exist
!               -2 data points in file exceed dimension
!-----
!  This routine reads waveform data written in SAC binary format.
!
!  Written by WQD, 2013/8/24 10:49:39
!-----
 implicit none
 integer*4::i,IRU,nerr
 logical::ext
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)
 character*(*)::name
!-----
!  Read real and integer header blocks to find actual number
!  of waveform data points which is stored in ihdr(10).
!-----
 inquire(file=name,exist=ext)
 if(.not.ext) then
  ihdr(10)=0
  nerr=-1
  return
 endif
 nerr=0
 !open(IRU,file=name,form='unformatted',access='direct',recl=440,status='old')
 !read(IRU,rec=1) (rhdr(i),i=1,70),(ihdr(i),i=1,40)
 open(IRU,file=name,form='unformatted',access='direct',recl=632,status='old')
 write(IRU,rec=1) (rhdr(i),i=1,70),(ihdr(i),i=1,40),(chdr(i),i=1,24)
 close(IRU)
 return
end subroutine


subroutine wsac(data,n,wname)
 implicit none
 integer*4::i,n,len
 real*4::min,max,sum
 real*4::data(n)
 character*(*)::wname
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)
 ihdr(10)=n
 min=1.0e+38
 max=-1.0e+38
 sum=0.0e0
 do i=1,ihdr(10)
	if(min.gt.data(i)) min=data(i)
	if(max.lt.data(i)) max=data(i)
  sum=sum+data(i)
 enddo
 rhdr(2)=min
 rhdr(3)=max
 rhdr(57)=sum/ihdr(10)
!  NZYEAR=ihdr(1) NZJDAY=ihdr(2) NZHOUR=ihdr(3) NZMIN=ihdr(4)
!  NZSEC=ihdr(5)  NZMSEC=ihdr(6)
!  ihdr(1)=year
!  ihdr(2)=day
 !print*, wname
 len=len_trim(wname)
 call bwsac(20,ihdr(10),wname(1:len),data)
 return
end subroutine


subroutine bwsac(IWU,LN,name,data)
!-----
!  This routine writes out a waveform data in SAC binary format.
!
!  Written by Hafidh A. A. Ghalib, 1988.
!
!  Modified by Wang, 2012
!-----
 implicit none
 integer*4::i,k,j,l,IWU,LN,nerr,nrec
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)
 character*(*)::name
 real*4::data(LN)
!-----
!  remove the original file so that the output length is
!  never greater than desired. Else the dregs of the
!  first will remain
!-----
 open(IWU,file=name,form='unformatted',access='sequential',status='unknown')
 rewind(IWU)
 close(IWU,status='delete')
 ihdr(10)=LN
!  The actual number of waveform data points is stored in integer
!  header 10. The file recored length is 158*4=632.
 nrec=632+4*ihdr(10)
 open(IWU,file=name,form='unformatted',access='direct',recl=nrec,status='unknown')
 write(IWU,rec=1) (rhdr(i),i=1,70),     &
                  (ihdr(k),k=1,40),     &
                  (chdr(j),j=1,24),     &
                  (data(l),l=1,LN)
 close(IWU)
 return
end subroutine


subroutine spline3(n,x,y,m,t,u)
!subroutine spline3(n,x,y,m,t,u,du,ddu,sum)
! 三次样条插值
! n = the number of the data samvles
! m = the number of the interpolation points you want to produce
! x(n)= epochs of samvle points
! y(n)= ddualue of function at samvles points
! t(m)= epochs of the interpolation points
! u(m)= ddualues of function at interpolation points
! du(m)= the first degree differential(dao suo)
! ddu(m)= the second degree differential(dao shu)
! sum = calculus(ji fen)
! a(n),b(n),c(n),d(n)= the working array
! reference: 丁月蓉，天文数据处理(for interpolation and differential)；
!            徐士良，fortran常用算法程序集(for calculus)。
 implicit none
 integer*4::n,m,i,j,j1,j2,j3,l
 real*4::e,f,rr,ss,tt,aa,dd,bb,cc
 real*4::x(n),y(n),t(m),u(m),a(n),b(n),c(n),d(n),du(m),ddu(m),sum(m)
 a(1)=0.0d0
 d(1)=0.0d0
 d(n)=0.0d0
 c(n)=0.0d0
 a(n)=1.0d0
 b(1)=1.0d0
 c(1)=-1.0d0
 b(n)=-1.0d0
 l=n-1
 do 5 i=2,l
  a(i)=(x(i)-x(i-1))/6.0d0
  c(i)=(x(i+1)-x(i))/6.0d0
  b(i)=2.0d0*(a(i)+c(i))
 5 d(i)=(y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1))/(x(i)-x(i-1))
 c(1)=c(1)/b(1)
 d(1)=d(1)/b(1)
 do 10 i=2,n
  c(i)=c(i)/(b(i)-a(i)*c(i-1))
 10 d(i)=(d(i)-a(i)*d(i-1))/(b(i)-a(i)*c(i-1))
 a(n)=d(n)
 do 15 i=1,l
  j=n-i
 15 a(j)=d(j)-c(j)*a(j+1)
 do 30 j1=1,m
  f=t(j1)
  do 20 j2=1,n-1
   if(x(j2).le.f.and.f.le.x(j2+1)) goto 25
  20 continue
  goto 30
  25 e=x(j2+1)-x(j2)
  rr=(a(j2)*(x(j2+1)-f)**3+a(j2+1)*(f-x(j2))**3)/6.0d0/e
  ss=(x(j2+1)-f)*(y(j2)/e-a(j2)*e/6.0d0)
  tt=(f-x(j2))*(y(j2+1)/e-a(j2+1)*e/6.0d0)
  aa=(a(j2+1)*(f-x(j2))**2)/2.0d0/e
  dd=(a(j2)*(x(j2+1)-f)**2)/2.0d0/e
  bb=(y(j2+1)-y(j2))/e
  cc=(a(j2+1)-a(j2))*e/6.0d0
  du(j1)=aa+bb-cc-dd
  u(j1)=rr+ss+tt
  ddu(j1)=(a(j2)*(x(j2+1)-f)+a(j2+1)*(f-x(j2)))/e
 30 continue
 do 33 i=1,m
  do 33 j3=1,i-1
   if(j3.ne.m) then
    e=t(j3+1)-t(j3)
   else
    e=t(m)-t(m-1)
   endif
   sum(i)=sum(i)+.5d0*e*(u(j3+1)+u(j3))-e**3*(ddu(j3)+ddu(j3+1))/24.0d0
 33 continue
 !print *, u
 return
end subroutine


subroutine espl1(x,y,n,dy1,dyn,xx,m,s)
!subroutine espl1(x,y,n,dy1,dyn,xx,m,dy,ddy,s,ds,dds,t,h)
!给定端点一阶导数的三次样条插值-徐士良
!x(n),y(n): 输入参数，n个结点值和函数值
!dy1,dyn: 输入参数，第一个结点和最后一个结点的一阶导数值
!xx(m): 输入参数，m个插值点值
!dy(n),ddy(n): 输出参数，n个给定结点处的一阶导数值和二阶导数值
!s(m),ds(m),dds(m):输出参数，m个指定插值点处函数值、一阶导数值和二阶导数值
!t: 输出参数，插值区间[x1,xn]间上的积分值
!h(n): 工作数组
 implicit none
 integer*4::n,m,j,i
 real*4::dy1,dyn,t,h0,h1,beta,alpha
 real*4::x(n),y(n),xx(m),dy(n),ddy(n),s(m),ds(m),dds(m),h(n)
 dy(1)=0.0e0
 h(1)=dy1
 h0=x(2)-x(1)
 do j=2,n-1
  h1=x(j+1)-x(j)
  alpha=h0/(h0+h1)
  beta=(1.0e0-alpha)*(y(j)-y(j-1))/h0
  beta=3.0e0*(beta+alpha*(y(j+1)-y(j))/h1)
  dy(j)=-alpha/(2.0e0+(1.0e0-alpha)*dy(j-1))
  h(j)=(beta-(1.0e0-alpha)*h(j-1))
  h(j)=h(j)/(2.0e0+(1.0e0-alpha)*dy(j-1))
  h0=h1
 enddo
 dy(n)=dyn
 do j=n-1,1,-1
  dy(j)=dy(j)*dy(j+1)+h(j)
 enddo
 do j=1,n-1
  h(j)=x(j+1)-x(j)
 enddo
 do j=1,n-1
  h1=h(j)*h(j)
  ddy(j)=6.0e0*(y(j+1)-y(j))/h1-2.0e0*(2.0e0*dy(j)+dy(j+1))/h(j)
 enddo
 h1=h(n-1)*h(n-1)
 ddy(n)=6.0e0*(y(n-1)-y(n))/h1+2.0e0*(2.0e0*dy(n)+dy(n-1))/h(n-1)
 t=0.0e0
 do i=1,n-1
  h1=0.5e0*h(i)*(y(i)+y(i+1))
  h1=h1-h(i)*h(i)*h(i)*(ddy(i)+ddy(i+1))/24.0e0
  t=t+h1
 enddo
 do 70 j=1,m
  if(xx(j).ge.x(n)) then
   i=n-1
  else
   i=1
 60 if (xx(j).gt.x(i+1)) then
    i=i+1
    goto 60
   endif
  endif
  h1=(x(i+1)-xx(j))/h(i)
  s(j)=(3.0e0*h1*h1-2.0e0*h1*h1*h1)*y(i)
  s(j)=s(j)+h(i)*(h1*h1-h1*h1*h1)*dy(i)
  ds(j)=6.0e0*(h1*h1-h1)*y(i)/h(i)
  ds(j)=ds(j)+(3.0e0*h1*h1-2.0e0*h1)*dy(i)
  dds(j)=(6.0e0-12.0e0*h1)*y(i)/(h(i)*h(i))
  dds(j)=dds(j)+(2.0e0-6.0e0*h1)*dy(i)/h(i)
  h1=(xx(j)-x(i))/h(i)
  s(j)=s(j)+(3.0e0*h1*h1-2.0e0*h1*h1*h1)*y(i+1)
  s(j)=s(j)-h(i)*(h1*h1-h1*h1*h1)*dy(i+1)
  ds(j)=ds(j)-6.0e0*(h1*h1-h1)*y(i+1)/h(i)
  ds(j)=ds(j)+(3.0e0*h1*h1-2.0e0*h1)*dy(i+1)
  dds(j)=dds(j)+(6.0e0-12.0e0*h1)*y(i+1)/(h(i)*h(i))
  dds(j)=dds(j)-(2.0e0-6.0e0*h1)*dy(i+1)/h(i)
 70 continue
 return
end subroutine


subroutine onebit(data,n)
 implicit none
 integer*4::i,n
 real*4::data(n)
 do i=1,n
  if(data(i).gt.0.0e0) then
   data(i)=1.0e0
  else if(data(i).lt.0.0e0) then
   data(i)=-1.0e0
  endif
 enddo
 return
end subroutine


subroutine runmean(data,nfft,logn,dt,wn,t1,t2,t3,t4)
!滑动绝对平均法(running-absolute-mean normalization),权基于带通滤波数据
!来自Processing seismic ambient noise data to obtain reliable
!    broad-band surface wave dispersion measurements    G.D.Bensen(2007)
 implicit none
 integer*4::i,j,nfft,logn,wn,wl,wr
 real*4::data(nfft),dataf(nfft),dt,t1,t2,t3,t4,w
 dataf=data
 call bpf(dataf,nfft,logn,dt,t1,t2,t3,t4)
 do i=1,nfft
  w=0.0e0
  wl=i-wn
  wr=i+wn
  if(wl.lt.1) wl=1
  if(wr.gt.nfft) wr=nfft
  !print*, wl,wr
  do j=wl,wr
   w=w+abs(dataf(j))
  enddo
  w=w/(wr-wl+1)
  data(i)=data(i)/w
 enddo
 return
end subroutine


subroutine sonebit(data,nfft,logn,nerr)
 implicit none
 integer*4::i,logn,nfft,nerr
 real*4::data(nfft),PR(nfft),PI(nfft),FR(nfft),FI(nfft)
 nerr=0
 PR=0.0e0
 PI=0.0e0
 FR=0.0e0
 FI=0.0e0
 PR=data
 call taper(PR,nfft,0.005)
 call KKFFT(PR,PI,nfft,logn,FR,FI,0,1)
 !open(14,file='ZZ')
 !do i=1,nn
  !write(14,*) PR(i),PI(i),FR(i),FI(i)
 !enddo
 !close(14)
 do i=1,nfft/2+1
  if(PR(i).eq.0.0e0) then
   nerr=nerr+1
   print*, i,PR(i),PI(i),FR(i),FI(i)
   FR(i)=1.0e0
   FI(i)=0.0e0
  else
   FR(i)=FR(i)/PR(i)
   FI(i)=FI(i)/PR(i)
  endif
 enddo
 PR=FR
 PI=FI
 PR(1)=PR(1)/2
 PI(1)=PI(1)/2
 PR(nfft/2+1)=PR(nfft/2+1)/2
 PI(nfft/2+1)=PI(nfft/2+1)/2
 PR(nfft/2+2:nfft)=0.0e0
 PI(nfft/2+2:nfft)=0.0e0       !令负频率为零
 call taper(PR,nfft,0.005)
 call taper(PI,nfft,0.005)
 call KKFFT(PR,PI,nfft,logn,FR,FI,1,0)
 !print*, FR(1:2)
 data=2*FR
 return
end subroutine


subroutine srunmean(data,nfft,logn,wn,nerr)
 implicit none
 integer*4::i,j,logn,nfft,wl,wr,wn,nerr
 real*4::data(nfft),PR(nfft),PI(nfft),FR(nfft),FI(nfft),w
 !open(11,file='Spectrum')
 nerr=0
 PR=0.0e0
 PI=0.0e0
 FR=0.0e0
 FI=0.0e0
 call taper(data,nfft,0.005)
 PR=data
 call KKFFT(PR,PI,nfft,logn,FR,FI,0,1)
 call smooth(PR,nfft)
 do i=1,nfft
  w=0.0e0
  wl=i-wn
  wr=i+wn
  if(wl.lt.1) wl=1
  if(wr.gt.nfft) wr=nfft
  !print*, wl,wr
  do j=wl,wr
   w=w+abs(PR(j))
  enddo
  w=w/(wr-wl+1)
  PR(i)=PR(i)/w
  !write(11,*) w,PR(i)
 enddo
 do i=1,nfft/2+1
  w=sqrt(FR(i)*FR(i)+FI(i)*FI(i))
  if(w.eq.0.0e0) then
   nerr=nerr+1
   print*, i,FR(i),FI(i)
   FR(i)=1.0e0
   FI(i)=0.0e0
  else
   FR(i)=FR(i)*PR(i)/w
   FI(i)=FI(i)*PR(i)/w
  endif
 enddo
 PR=FR
 PI=FI
 !close(11)
 PR(1)=PR(1)/2
 PI(1)=PI(1)/2
 PR(nfft/2+1)=PR(nfft/2+1)/2
 PI(nfft/2+1)=PI(nfft/2+1)/2
 PR(nfft/2+2:nfft)=0.0e0
 PI(nfft/2+2:nfft)=0.0e0       !令负频率为零
 call taper(PR,nfft,0.005)
 call taper(PI,nfft,0.005)
 call KKFFT(PR,PI,nfft,logn,FR,FI,1,0)
 data=2*FR
 return
end subroutine


subroutine MFT(data,n,dt,TC,alpha,dataE,dataIP)
! 多重滤波程序(窄带滤波器为高斯滤波器,截断为pai=3.14159265357e0)
! data:输入输出-波形数据
! dataE:输出-波形振幅(包络)
! dataIP:输出-(瞬时相位,瞬时周期)
 implicit none
 integer*4::i,n,logn,nfft
 real*4::data(n),dataE(n),dataIP(n,2)
 real*4::dt,TC,fc,df,f,alpha,fac,freqlw,frequp,G,s
 real*4,parameter::pai=3.14159265358e0
 real*4,allocatable::PR(:),PI(:),FR(:),FI(:),PRD(:),PID(:),FRD(:),FID(:)
 call npow2(n,nfft,logn)
 fc=1.0e0/TC
 df=1.0e0/(nfft*dt)
 !print*, 'logn=',logn,'nfft=',nfft
 allocate(PR(nfft),PI(nfft),FR(nfft),FI(nfft))
 allocate(PRD(nfft),PID(nfft),FRD(nfft),FID(nfft))
 PR=0.0e0
 PI=0.0e0
 FR=0.0e0
 FI=0.0e0
 PRD=0.0e0
 PID=0.0e0
 FRD=0.0e0
 FID=0.0e0
 call taper(data,n,0.005)
 PR(1:n)=data(1:n)
 call KKFFT(PR,PI,nfft,logn,FR,FI,0,0)
 fac=sqrt(pai/alpha)
 frequp=(1.0e0+fac)*fc
 freqlw=(1.0e0-fac)*fc
 do i=1,nfft/2+1
  f=(i-1)*df
  s=2*pai*f
  if(f.ge.freqlw.and.f.le.frequp) then
   G=exp(-alpha*(f/fc-1.0e0)**2)
  else
   G=0.0e0
  endif
  PR(i)=FR(i)*G
  PI(i)=FI(i)*G
  PRD(i)=-s*PI(i)
  PID(i)=s*PR(i)
 enddo
 PR(1)=PR(1)/2
 PI(1)=PI(1)/2
 PR(nfft/2+1)=PR(nfft/2+1)/2
 PI(nfft/2+1)=PI(nfft/2+1)/2
 PR(nfft/2+2:nfft)=0.0e0
 PI(nfft/2+2:nfft)=0.0e0       !令负频率为零
 PRD(1)=PRD(1)/2
 PID(1)=PID(1)/2
 PRD(nfft/2+1)=PRD(nfft/2+1)/2
 PID(nfft/2+1)=PID(nfft/2+1)/2
 PRD(nfft/2+2:nfft)=0.0e0
 PID(nfft/2+2:nfft)=0.0e0       !令负频率为零
 call taper(PR,nfft,0.005)
 call taper(PI,nfft,0.005)
 call KKFFT(PR,PI,nfft,logn,FR,FI,1,1)
 data(1:n)=2*FR(1:n)
 dataE(1:n)=PR(1:n)
 dataIP(1:n,1)=PI(1:n)!*2*pai/360.0e0
 call taper(PRD,nfft,0.005)
 call taper(PID,nfft,0.005)
 call KKFFT(PRD,PID,nfft,logn,FRD,FID,1,0)
 do i=1,n
  dataIP(i,2)=(FR(i)*FID(i)-FRD(i)*FI(i))/(dataE(i)**2)
  dataIP(i,2)=2*pai/dataIP(i,2)
 enddo
 deallocate(PR,PI,FR,FI,PRD,PID,FRD,FID)
 return
end subroutine


subroutine MFTC(data,n,dt,tc,alpha,minhf0)
! 多重滤波程序(窄带滤波器为高斯滤波器,截断为pai=3.14159265358e0)
! data:输入输出-波形数据
 implicit none
 integer*4::i,n,logn,nfft
 real*4::data(n)
 real*4::dt,tc,fc,df,hf,f,alpha,fac,freqlw,frequp,G,minhf0
 real*4,parameter::pai=3.14159265358e0
 real*4,allocatable::PR(:),PI(:),FR(:),FI(:)
 call npow2(n,nfft,logn)
 fc=1.0e0/tc
 df=1.0e0/(nfft*dt)
 fac=sqrt(pai/alpha)
 frequp=(1.0e0+fac)*fc
 freqlw=(1.0e0-fac)*fc
 hf=fac*fc
 do while(df.gt.minhf0)
  nfft=2*nfft
  logn=logn+1
  df=1.0e0/(nfft*dt)
 enddo
 !print*, 'logn=',logn,'nfft=',nfft
 allocate(PR(nfft),PI(nfft),FR(nfft),FI(nfft))
 PR=0.0e0
 PI=0.0e0
 FR=0.0e0
 FI=0.0e0
 call taper(data,n,0.5)
 PR(1:n)=data(1:n)
 call KKFFT(PR,PI,nfft,logn,FR,FI,0,0)
 do i=1,nfft/2+1
  f=(i-1)*df
  if(f.ge.freqlw.and.f.le.frequp) then
   G=exp(-alpha*(f/fc-1.0e0)**2)
  else
   G=0.0e0
  endif
  PR(i)=FR(i)*G
  PI(i)=FI(i)*G
 enddo
 PR(1)=PR(1)/2
 PI(1)=PI(1)/2
 PR(nfft/2+1)=PR(nfft/2+1)/2
 PI(nfft/2+1)=PI(nfft/2+1)/2
 PR(nfft/2+2:nfft)=0.0e0
 PI(nfft/2+2:nfft)=0.0e0       !令负频率为零
 call taper(PR,nfft,0.5)
 call taper(PI,nfft,0.5)
 call KKFFT(PR,PI,nfft,logn,FR,FI,1,0)
 data(1:n)=2*FR(1:n)
 deallocate(PR,PI,FR,FI)
 return
end subroutine


subroutine SUF(data,dataE,dataIP,n,TC,B,DT,DIST,dir,snr,gv,ip,it,nerr)
! dir=1 : 计算因果信号的信噪比
! dir=-1: 计算非因果信号的信噪比
 implicit none
 integer*4::i,n,dir,sl,sr,nl,nr,maxI,nerr
 real*4::data(n),dataE(n),dataIP(n,2),TC,B,DT,DIST,maxE
 real*4::speak,nrms,srms,snr,gv,ip,it
 real*4,parameter::Umin=2.0e0,Umax=5.0e0
 !real*4,parameter::Tmin=1.0e0,Tmax=50.0e0
 nerr=0
 if(dir.ne.1.and.dir.ne.-1) then
  nerr=1
  return
 endif
 !sl=floor(((DIST/Umax-Tmax)*dir-B)/DT)
 sl=floor((DIST/Umax*dir-B)/DT)
 !if(sl.lt.1) sl=1
 !sr=floor(((DIST/Umin+2*Tmin)*dir-B)/DT)+1
 sr=floor((DIST/Umin*dir-B)/DT)+1
 !print*, sl,sr,dir
 speak=0.0e0
 maxE=0.0e0
 do i=sl,sr,dir
  if(speak.lt.abs(data(i))) speak=abs(data(i))
  if(maxE.lt.dataE(i)) then
   maxE=dataE(i)
   maxI=i
  endif
 enddo
 !print*, maxI
 gv=DIST/(B+(maxI-1)*DT)*dir            !群速度
 ip=dataIP(maxI,1)                      !瞬时相位
 it=dataIP(maxI,2)                      !瞬时周期
 nl=floor(-B/DT)+1
 nr=maxI-anint(1.5e0*TC/DT)*dir
 !print*, nl,nr,dir
 if((nr-maxI)*(nr-nl).gt.0) then
  snr=-99999.0e0
  return
 endif
 srms=0.0e0
 sl=nr
 sr=maxI+anint(1.5e0*TC/DT)*dir
 do i=sl,sr,dir
  srms=srms+data(i)**2
 enddo
 srms=sqrt(srms/((sr-sl)*dir+1))
 nrms=0.0e0
 do i=nl,nr,dir
  nrms=nrms+data(i)**2
 enddo
 nrms=sqrt(nrms/((nr-nl)*dir+1))
 snr=speak/nrms                       !信噪比
 !snr=srms/nrms
 return
end subroutine


subroutine SUF-old(data,dataE,dataIP,n,B,DT,DIST,dir,ratio,gv,ip,it,nerr)
! dir=1 : 计算因果信号的信噪比
! dir=-1: 计算非因果信号的信噪比
 implicit none
 integer*4::i,n,dir,sl,sr,nl,nr,maxI,nerr
 real*4::data(n),dataE(n),dataIP(n,2),B,DT,DIST,maxE
 real*4::speak,nrms,ratio,gv,ip,it
 real*4,parameter::Umin=2.0e0,Umax=5.0e0
 !real*4,parameter::Tmin=1.0e0,Tmax=50.0e0
 nerr=0
 if(dir.ne.1.and.dir.ne.-1) then
  nerr=1
  return
 endif
 !sl=floor(((DIST/Umax-Tmax)*dir-B)/DT)
 sl=floor((DIST/Umax*dir-B)/DT)
 !if(sl.lt.1) sl=1
 !sr=floor(((DIST/Umin+2*Tmin)*dir-B)/DT)+1
 sr=floor((DIST/Umin*dir-B)/DT)+1
 nl=sr+anint(500.0e0/DT)*dir
 if(nl.gt.n.or.nl.lt.1) then
  nerr=2
  return
 endif
 nr=nl+anint(500.0e0/DT)*dir
 if(nr.gt.n) nr=n
 if(nr.lt.1) nr=1
 !print*, sl,sr
 speak=0.0e0
 maxE=0.0e0
 do i=sl,sr,dir
  if(speak.lt.abs(data(i))) speak=abs(data(i))
  if(maxE.lt.dataE(i)) then
   maxE=dataE(i)
   maxI=i
  endif
 enddo
 gv=DIST/(B+(maxI-1)*DT)*dir
 ip=dataIP(maxI,1)
 it=dataIP(maxI,2)
 nrms=0.0e0
 do i=nl,nr,dir
  nrms=nrms+data(i)**2
 enddo
 nrms=sqrt(nrms/((nr-nl)*dir+1))
 ratio=speak/nrms
 return
end subroutine


subroutine Jzqn1(ZA,N,ZAF)
!矩阵求逆子程序
	  IMPLICIT NONE
    INTEGER*4::N,K,I,J
    REAL*8::ZA(N,N),ZAF(N,N),G(N,N+N),B,C
	  DO K=1,N
	    DO J=1,N
	      G(K,J)=ZA(K,J)
	      G(K,J+N)=0.0E0
	      IF(J.EQ.K) G(K,J+N)=1
      ENDDO
    ENDDO
    DO K=1,N
	    DO I=K,N
	      IF(G(I,K).NE.0.0E0) GOTO 50
      ENDDO
      RETURN
50    DO J=K,2*N
        B=G(K,J)
	      G(K,J)=G(I,J)
	      G(I,J)=B
      ENDDO
      C=1.0E0/G(K,K)
	    DO J=K,2*N
	      G(K,J)=C*G(K,J)
      ENDDO
      DO I=1,N
	      IF(I.NE.K) THEN
	        C=-G(I,K)
	        DO J=K,2*N
	          G(I,J)=G(I,J)+C*G(K,J)
          ENDDO
        ENDIF
      ENDDO
    ENDDO
    DO I=1,N
	    DO J=1,N
	      ZAF(I,J)=G(I,J+N)
      ENDDO
    ENDDO
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


subroutine GetDistAz2(lat1,long1,lat2,long2,dist,az)
!由两点经纬度计算大圆劣弧长和方位角
!longitude:经度;latitude:纬度
!东经为正,西经为负,北纬为正,南纬为负
!1为观测点,2为目标点,az为1相对2的方位角;
!1为事件,2为台站, az为事件到台站的方位角;
 real*4::lat1,long1,lat2,long2,delta,dist,az
 real*4::radlat1,radlong1,radlat2,radlong2,a,b,colat
 real*4::pi,pi2,rad,flat,predel,xtop,xden,radius,EARTH_RADIUS
 data EARTH_RADIUS/6371.004e0/ !平均半径
 !data EARTH_RADIUS/6378.137e0/ !赤道半径
 data pi/3.1415926535898e0/
 data flat/0.993231e0/
 pi2=pi/2
 rad=pi/180.0e0
!-----convert to radians
 radlat1=lat1*rad
 radlong1=long1*rad
 radlat2=lat2*rad
 radlong2=long2*rad
!-----convert latitudes to geocentric colatitudes
 radlat1=pi2-atan(flat*tan(radlat1))
 radlat2=pi2-atan(flat*tan(radlat2))
!-----calculate delta (rad)
 a=radlat1-radlat2
 b=radlong1-radlong2
 !delta=acos(cos(radlat1)*cos(radlat2)+sin(radlat1)*sin(radlat2)*cos(b))
 predel=sqrt(sin(a/2)*sin(a/2)+sin(radlat1)*sin(radlat2)*sin(b/2)*sin(b/2))
 if(abs(predel+1.0e0).lt.0.000001e0) then
  predel=-1.0e0
 endif
 if(abs(predel-1.0e0).lt.0.000001e0) then
  predel=1.0e0
 endif
 delta=2*asin(predel)
!-----calculate azimuth from 1 to 2
 xtop=sin(b)
 xden=(sin(radlat1)/tan(radlat2))-cos(b)*cos(radlat1)
 az=atan2(xtop,xden)/rad
 if(az.lt.0.0e0) az=360.0e0+az
 if(abs(az-360.0e0).lt.0.00001e0) az=0.0e0
!-----compute distance in kilometers
 colat=pi2-(lat1+lat2)/2*rad
 radius=6378.163e0*(1.e0+3.35278e0-3*((1.e0/3.e0)-(cos(colat)**2)))
 dist=delta*radius
 return
end subroutine


subroutine delaz(alat,alon,blat,blon,del,dist,az)
! Compute distance and azimuth on a sphere
!	computes distance and azimuth from a to b
!	a and b are in decimal degrees and n-e coordinates
!	del  -- delta in degrees
!	dist -- distance in km
!	az   -- azimuth from a to b clockwise from north in degrees
!	Original author:  Bruce Julian
!	new values for radius used (from Edi Kissling)
 implicit none
!	Parameters:
 real*4::alat,alon	! Coordinates of first point
 real*4::blat,blon	! Coordinates of second point
 real*4::del		    ! Central angle (degrees)
 real*4::dist		    ! Distance (km)
 real*4::az		      ! Azimuth from a to b (degrees)
 real*4::xtop,xden
!	Local variables:
 real*8::azr,acol,bcol,alatr,alonr,blatr,blonr,colat,cosdel
 real*8::flat,rad,pi2
 real*8::delr,geoa,geob,radius,diflon,tana,tanb
!	Built-in functions:  Declarations not needed
 !real*8::dtan,datan,dsin,dcos,dacos
 !real*8::top,den
 data pi2/1.570796d0/
 data rad/1.745329d-02/
 data flat/.993231d0/
!-----convert to radians
 alatr=alat*rad
 alonr=alon*rad
 blatr=blat*rad
 blonr=blon*rad
!-----convert latitudes to geocentric colatitudes
 tana=flat*dtan(alatr)
 geoa=datan(tana)
 acol=pi2-geoa
 tanb=flat*dtan(blatr)
 geob=datan(tanb)
 bcol=pi2-geob
!-----calculate delta
 diflon=blonr-alonr
 cosdel=dsin(acol)*dsin(bcol)*dcos(diflon)+dcos(acol)*dcos(bcol)
 delr=dacos(cosdel)
!-----calculate azimuth from a to b
!*****	Note the use of single precision xtop and xden instead
!	of the double precision top and den in the original program.
!*****	Note also the call to atan2 instead of datan2.
!	Both of these changes were made so that dyn.load
!	would work in Splus.  For some reason, the ld command
!	ld -r -d didn't find _d_atan2
!						WLE 10/16/91
 xtop=dsin(diflon)
 xden=(dsin(acol)/dtan(bcol))-dcos(diflon)*dcos(acol)
 azr=atan2(xtop,xden)
!----- convert to degrees
 del=delr/rad
 az=azr/rad
 if(az.lt.0.0e0) az=360.0d0+az
!-----compute distance in kilometers
 colat=pi2-(alatr+blatr)/2.d0
 radius=6378.163d0*(1.d0+3.35278d-3*((1.d0/3.d0)-(dcos(colat)**2)))
 dist=delr*radius
 return
end subroutine


subroutine azdist(stalat,stalon,evtlat,evtlon,delta,az,baz)
!
! Subroutine to calculate the Great Circle Arc distance
!    between two sets of geographic coordinates
!
! Given:  stalat => Latitude of first point (+N, -S) in degrees
!	        stalon => Longitude of first point (+E, -W) in degrees
!	        evtlat => Latitude of second point
!	        evtlon => Longitude of second point
!
! Returns:  delta => Great Circle Arc distance in degrees
!	          az    => Azimuth from pt. 1 to pt. 2 in degrees
!	          baz   => Back Azimuth from pt. 2 to pt. 1 in degrees
!
! If you are calculating station-epicenter pairs, pt. 1 is the station
!
! Equations take from Bullen, pages 154, 155
!
! T. Owens, September 19, 1991
!           Sept. 25 -- fixed az and baz calculations
!           Dec. 2006, changed for fortran95
!           May, 2007 -- added predel to get around OSX acos round-off NaN issue
!
      double precision scolat,slon,ecolat,elon
      double precision a,b,c,d,e,aa,bb,cc,dd,ee,g,gg,h,hh,k,kk
      double precision rhs1,rhs2,sph,rad,del,daz,dbaz,pi

      pi=3.141592654
      piby2=pi/2.
      rad=2.*pi/360.

! scolat and ecolat are the geocentric colatitudes
! as defined by Richter (pg. 318)
!
! Earth Flattening of 1/298.257 take from Bott (pg. 3)

      sph=1.0/298.257

      scolat=piby2-atan((1.-sph)*(1.-sph)*tan(dble(stalat)*rad))
      ecolat=piby2-atan((1.-sph)*(1.-sph)*tan(dble(evtlat)*rad))
      slon=dble(stalon)*rad
      elon=dble(evtlon)*rad

!  a - e are as defined by Bullen (pg. 154, Sec 10.2)
!     These are defined for the pt. 1

      a=sin(scolat)*cos(slon)
      b=sin(scolat)*sin(slon)
      c=cos(scolat)
      d=sin(slon)
      e=-cos(slon)
      g=-c*e
      h=c*d
      k=-sin(scolat)

!  aa - ee are the same as a - e, except for pt. 2

      aa=sin(ecolat)*cos(elon)
      bb=sin(ecolat)*sin(elon)
      cc=cos(ecolat)
      dd=sin(elon)
      ee=-cos(elon)
      gg=-cc*ee
      hh=cc*dd
      kk=-sin(ecolat)

!  Bullen, Sec 10.2, eqn. 4

      predel=a*aa+b*bb+c*cc
      if(abs(predel+1.).lt..000001) then
        predel=-1.
      endif
      if(abs(predel-1.).lt..000001) then
        predel=1.
      endif
      del=acos(predel)
      delta=del/rad

!  Bullen, Sec 10.2, eqn 7 / eqn 8
!
!    pt. 1 is unprimed, so this is technically the baz
!
!  Calculate baz this way to avoid quadrant problems

      rhs1=(aa-d)*(aa-d)+(bb-e)*(bb-e)+cc*cc-2.
      rhs2=(aa-g)*(aa-g)+(bb-h)*(bb-h)+(cc-k)*(cc-k)-2.
      dbaz=atan2(rhs1,rhs2)
      if(dbaz.lt.0.0d0) dbaz=dbaz+2*pi
      baz=dbaz/rad

!  Bullen, Sec 10.2, eqn 7 / eqn 8
!
!    pt. 2 is unprimed, so this is technically the az

      rhs1=(a-dd)*(a-dd)+(b-ee)*(b-ee)+c*c-2.
      rhs2=(a-gg)*(a-gg)+(b-hh)*(b-hh)+(c-kk)*(c-kk)-2.
      daz=atan2(rhs1,rhs2)
      if(daz.lt.0.0d0) daz=daz+2*pi
      az=daz/rad

!   Make sure 0.0 is always 0.0, not 360.

      if(abs(baz-360.).lt..00001) baz=0.0
      if(abs(az-360.).lt..00001) az=0.0
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


SUBROUTINE HPIR1(X,Y,N,A,M,DT1,DT2,DT3)
!最小二乘曲线拟合,来自徐士良《Fortran常用算法程序集-第二版》
!(X(N),Y(N)):数据坐标,A(M):拟合的M-1次多项式的系数
!拟合的多项式为 Y=A(0)+A(1)*(X-Xmean)+...+A(M-1)*(X-Xmean)^(M-1)
!DT1,DT2,DT3:拟合多项式与数据点偏差的平方和,绝对值之和,绝对值的最大值
 IMPLICIT NONE
 INTEGER*4::N,M,I,J,K
 REAL*4::X(N),Y(N),A(M),S(20),T(20),B(20)
 REAL*4::DT1,DT2,DT3,Z,D1,P,C,D2,G,Q,DT
 A=0.0E0
 IF(M.GT.N) M=N
 IF(M.GT.20) M=20
 Z=0.0E0
 DO I=1,N
  Z=Z+X(I)/N
 ENDDO
 B(1)=1.0E0
 D1=N
 P=0.0E0
 C=0.0E0
 DO I=1,N
  P=P+(X(I)-Z)
	C=C+Y(I)
 ENDDO
 C=C/D1
 P=P/D1
 A(1)=C*B(1)
 IF(M.GT.1) THEN
	T(2)=1.0E0
	T(1)=-P
	D2=0.0E0
	C=0.0E0
	G=0.0E0
	DO I=1,N
	 Q=X(I)-Z-P
	 D2=D2+Q*Q
	 C=Y(I)*Q+C
	 G=(X(I)-Z)*Q*Q+G
  ENDDO
	C=C/D2
	P=G/D2
	Q=D2/D1
	D1=D2
	A(2)=C*T(2)
	A(1)=C*T(1)+A(1)
 ENDIF
 DO J=3,M
	S(J)=T(J-1)
	S(J-1)=-P*T(J-1)+T(J-2)
	IF(J.GE.4) THEN
	 DO K=J-2,2,-1
	  S(K)=-P*T(K)+T(K-1)-Q*B(K)
   ENDDO
	ENDIF
	S(1)=-P*T(1)-Q*B(1)
	D2=0.0E0
	C=0.0E0
	G=0.0E0
	DO I=1,N
	 Q=S(J)
	 DO K=J-1,1,-1
    Q=Q*(X(I)-Z)+S(K)
   ENDDO
	 D2=D2+Q*Q
	 C=Y(I)*Q+C
	 G=(X(I)-Z)*Q*Q+G
  ENDDO
	C=C/D2
	P=G/D2
	Q=D2/D1
	D1=D2
	A(J)=C*S(J)
	T(J)=S(J)
	DO K=J-1,1,-1
	 A(K)=C*S(K)+A(K)
	 B(K)=T(K)
	 T(K)=S(K)
  ENDDO
 ENDDO
 DT1=0.0E0
 DT2=0.0E0
 DT3=0.0E0
 DO I=1,N
	Q=A(M)
	DO K=M-1,1,-1
   Q=Q*(X(I)-Z)+A(K)
  ENDDO
	DT=Q-Y(I)
	IF(ABS(DT).GT.DT3) DT3=ABS(DT)
	DT1=DT1+DT*DT
	DT2=DT2+ABS(DT)
 ENDDO
 RETURN
END SUBROUTINE


subroutine GetAngle(xa,ya,xb,yb,xc,yc,BAC)
!由三点经纬度计算两条弧线在球面上的顺时针交角
! xa, ya （两条弧线相交点的经度和纬度）
! xb, yb （起始弧线的起始点的经度和纬度）
! xc, yc （终止弧线的终点的经度和纬度）
! BAC    （所求球面角）
 real*4::xa,ya,xb,yb,xc,yc,BAC,a,b,c,d
 real*4,parameter::pi=3.1415926535898e0
 xa=xa*pi/180
 ya=ya*pi/180
 xb=xb*pi/180
 yb=yb*pi/180
 xc=xc*pi/180
 yc=yc*pi/180
 a=acos(cos(yb)*cos(yc)*cos(xb-xc)+sin(yb)*sin(yc))!求圆心角（弧度表示）
 b=acos(cos(ya)*cos(yc)*cos(xa-xc)+sin(ya)*sin(yc))!求圆心角（弧度表示）
 c=acos(cos(ya)*cos(yb)*cos(xa-xb)+sin(ya)*sin(yb))!求圆心角（弧度表示）
 d=(cos(a)-cos(b)*cos(c))/(sin(b)*sin(c))
 BAC=acos(d)*180/pi!求弧线夹角（单位度）
 return
end subroutine


function isRunNian(year)
!判断是否闰年:能被4整除但不能被100整除以及能被400整除的年份为闰年。
logical::isRunNian
integer::year
  if(mod(year,4)/=0) then
     isRunNian=.false.
  else if(mod(year,100)/=0) then
     isRunNian=.true.
  else if(mod(year,400)==0) then
     isRunNian=.true.
  else
     isRunNian=.false.
  endif
end function


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




