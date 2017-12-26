!对多个hypoDD重定位结果文件进行分检,得到一个事件的多次定位结果,用于分析定位误差

program sloc
 implicit none
 integer*4::i,j,k,k1,k2,k3,m,n,nloc,neve,ifindi
 integer*4,allocatable::id(:,:),ido(:),nl(:),ia(:)
 real*4::rtmp
 character::evefile*60,name*60,cid*20
 character,allocatable::loc(:)*60,card(:,:)*200

 open(9,file='sloc.log')

 open(10,file='sloc.in')
 call filelen(10,m)
 print*, m
 read(10,*) evefile   !事件信息文件
 nloc=m-1
 allocate(loc(nloc))
 do i=1,nloc
  read(10,*) loc(i)
 enddo
 close(10)

 open(11,file=evefile,status='old')
 call filelen(11,neve)
 print*, neve
 allocate(nl(nloc),card(neve,nloc),id(neve,nloc))

 do i=1,nloc
  open(12,file=loc(i),status='old')
  call filelen(12,nl(i))
  m=1
  do j=1,nl(i)
   k=0
   read(12,'(a200)') card(m,i)
   !print*, trim(card(m,i))
   !i=len_trim(card)
   !k=0
   !do j=1,i
    !if(ichar(card(j:j))>57.or.ichar(card(j:j))==42) then
     !print*, trim(card)
     !k=1
     !cycle
    !endif
   !enddo
   !if(k==1) cycle
   k1=index(card(m,i),'*')
   k2=index(card(m,i),'N')
   k3=index(card(m,i),'n')
   if(k1/=0.or.k2/=0.or.k3/=0) then
    print*, 'error file: ',trim(loc(i))
    write(9,*) 'error file: ',trim(loc(i)),card(m,i)(1:10)
    cycle
   endif
   !print*, trim(card(m,i))
   read(card(m,i),*) id(m,i),rtmp,rtmp,rtmp
   if(rtmp>60.0e0) then
    print*, 'error depth: ',trim(loc(i))
    write(9,*) 'error depth: ',trim(loc(i)),card(m,i)(1:10)
    cycle  !剔除异常定位结果
   endif
   m=m+1
  enddo
  nl(i)=m-1
  close(12)
 enddo

 print*,
 write(9,*)

 allocate(ido(neve))
 do i=1,neve
  read(11,*) k,k,rtmp,rtmp,rtmp,rtmp,rtmp,rtmp,rtmp,ido(i)
  n=0
  write(cid,'(i10)') ido(i)
  name='id_'//trim(adjustl(cid))//'.txt'
  !print*,name
  open(13,file=name)
  do j=1,nloc
   m=nl(j)
   !allocate(ia(m))
   !ia(1:m)=id(1:m,j)
   !call sorti(m,ia)
   !k=ifindi(m,ia,ido(i))
   do k=1,m
    if(id(k,j)==ido(i)) then
     write(13,'(a200)') card(k,j)
     n=n+1
     exit
    endif
   enddo
   !deallocate(ia)
  enddo
  if(n==0) then
   close(13,status='delete')
   print*, 'No loc for id',ido(i)
   write(9,*) 'No loc for id',ido(i)
  else
   close(13)
   print*, 'id: ',ido(i),n
   write(9,*) 'id: ',ido(i),n
  endif
 enddo
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


integer function ifindi(n,ia,iv)
! Find specified value in ordered integer vector
! ia(n): [1..n] Vector to search
! iv: Value to find
 implicit none
 integer*4::n,ia(n),iv,i,k
 if(n.le.0) then
  ifindi=0
  return
 endif
 if(iv.lt.ia(1).or.iv.gt.ia(n)) then
  ifindi=0
  return
 endif
 k=2
 i=nint(real(n)/k)
 10 if(k.gt.2*n) then  !Value not in vector
  ifindi=0
  return
 endif
 k=k*2
 if(iv.lt.ia(i)) then  !Value smaller:  Search below
  i=i-nint(real(n)/k)
  goto 10
 endif
 if(iv.gt.ia(i)) then  !Value larger:  Search above
  i=i+nint(real(n)/k)
  goto 10
 endif
 ifindi=i  !Value found: iv==ia[i]
 return
end function


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
