character*8 ar,re,line
character*1 it0,it1
real amat1(1000),amat_line(1000),zsharp(20)
integer kmat_line(1000)
real vpnew(300),vsnew(300),hnew(300)

allocatable x(:),x0(:),u(:),v(:),w(:),aaa(:),dt(:),xmod(:)
allocatable ncolrow(:),ncol(:)

common/refmod/nrefmod,hmod(600),vmodp(600),vmods(600)


open(1,file='../../model.dat')
read(1,'(a8)')re		! code of the area
read(1,'(a8)')ar		! code of the area
read(1,*)iter
close(1)
write(it0,'(i1)')iter-1
write(it1,'(i1)')iter
write(*,*)' area=',re,' model=',ar,' iter=',iter

open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/1Dmod_PARAM.DAT')
read(1,*)
read(1,*)zmin,zmax,dzstep,nev
read(1,*)
read(1,*)dz_par
read(1,*)sm_p,sm_s
read(1,*)rg_p,rg_s
read(1,*)w_hor,w_ver,w_time
read(1,*)iter_lsqr
read(1,*)nsharp
read(1,*)(zsharp(i),i=1,nsharp)
close(1)

if(iter.eq.1) then
	open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/ref_start.dat')
else
	open(1,file='../../DATA/'//re//'/'//ar//'/RESULT/ref'//it0//'.dat')
end if
read(1,*,end=81)vpvs
i=0
82	i=i+1
	read(1,*,end=81)hmod(i),vmodp(i),vs
	if(vpvs.lt.0.000001) then
		vmods(i)=vs
	else
		vmods(i)=vmodp(i)/vpvs
	end if
	!write(*,*)hmod(i),vmodp(i),vmods(i)
goto 82
81	close(1)
nrefmod=i-1
!write(*,*)' nrefmod=',nrefmod

if(iter.eq.1) then
	open(21,file='../../FIG_files/1dmod/ref_start.bln')
	write(21,*)nrefmod
	do i=1,nrefmod
		write(21,*)vmodp(i),-hmod(i)
	end do
	write(21,*)nrefmod
	do i=1,nrefmod
		write(21,*)vmods(i),-hmod(i)
	end do
	close(21)
end if



open(1,file='../../DATA/'//re//'/'//ar//'/TIMES/numb_1d.dat')
read(1,*)nray,nonz,nvel,nzt
close(1)
write(*,*)' nray=',nray,' nonz=',nonz

nonzer = nonz+nray*4
nrows = nray
ncols = nvel*2 + nzt*4


if(sm_p.gt.0.0001) then
	nonzer = nonzer + (nvel-1-nsharp) * 2
	nrows = nrows + (nvel-1-nsharp)
end if

if(sm_s.gt.0.0001) then
	nonzer = nonzer + (nvel-1-nsharp) * 2
	nrows = nrows + (nvel-1-nsharp)
end if

if(rg_p.gt.0.0001) then
	nonzer = nonzer + nvel 
	nrows = nrows + nvel
end if
if(rg_s.gt.0.0001) then
	nonzer = nonzer + nvel 
	nrows = nrows + nvel
end if

write(*,*)' nrows=',nrows,' ncols=',ncols,' nonzer=',nonzer

allocate(xmod(ncols),x(ncols),x0(ncols),v(ncols),w(ncols))
allocate(ncol(nrows),dt(nrows),u(nrows))
allocate(aaa(nonzer),ncolrow(nonzer))

open(1,file='../../TMP/matr_1d.dat',form='binary')
ir=0
nonz=0

43	continue
	read(1,end=42)nuz,res,ips
	read(1)nzt,dtdx,dtdy,dtdz
	do i=1,nuz
		read(1)amat_line(i),kmat_line(i)
	end do

	ir=ir+1

	dt(ir)=res			!!!!!!!!!!!!!!!!!!!!!!!!!
	ncol(ir) = nuz + 4

	n0=0
	if(ips.eq.2) n0=nvel

	dtsyn=0
	do ii=1,nuz
		nonz=nonz+1
		aaa(nonz) = amat_line(ii)
		ncolrow(nonz)= n0 + kmat_line(ii)

		!par=0.
		!zcur=(kmat_line(ii)-1)*dz_par
		!if((zcur-20)*(zcur-50).lt.0.) par=0.1
		!dtsyn=dtsyn+par*amat_line(ii)

	end do
	!dt(ir)=dtsyn
	!write(*,*)nuz,dtsyn,ips

	nonz=nonz+1
	aaa(nonz) = dtdx * w_hor
	ncolrow(nonz)= nvel*2+(nzt-1)*4+1
	nonz=nonz+1
	aaa(nonz) = dtdy * w_hor
	ncolrow(nonz)= nvel*2+(nzt-1)*4+2
	nonz=nonz+1
	aaa(nonz) = dtdz * w_ver
	ncolrow(nonz)= nvel*2+(nzt-1)*4+3
	nonz=nonz+1
	aaa(nonz) = w_time
	ncolrow(nonz)= nvel*2+(nzt-1)*4+4

	goto 43
42 close(1)
write(*,*)' ir=',ir,' nonz=',nonz


if(sm_p.gt.0.0001) then
	do i=1,nvel-1
		z1=(i-1)*dz_par
		z2=i*dz_par
		if(nsharp.ne.0) then
			do ish=1,nsharp
				if(zsharp(ish).eq.z1) goto 339
				if((zsharp(ish)-z1)*(zsharp(ish)-z2).lt.0) then
					write(*,*)' P sharp:',i,' z1=',z1,' z2=',z2
					goto 339
				end if
			end do
		end if

		ir=ir+1
		dt(ir)=0.
		ncol(ir)=2
		!write(*,*)' ir=',ir,' y1=',y1,' y2=',y2

		nonz=nonz+1
		aaa(nonz)=sm_p
		ncolrow(nonz) = i

		nonz=nonz+1
		aaa(nonz)=-sm_p
		ncolrow(nonz) = i+1
339 continue
	end do
end if

if(sm_s.gt.0.0001) then
	do i=1,nvel-1
		z1=(i-1)*dz_par
		z2=i*dz_par
		if(nsharp.ne.0) then
			do ish=1,nsharp
				if(zsharp(ish).eq.z1) goto 338
				if((zsharp(ish)-z1)*(zsharp(ish)-z2).lt.0) then
					write(*,*)' S sharp:',i,' z1=',z1,' z2=',z2
					goto 338
				end if
			end do
		end if


		ir=ir+1
		dt(ir)=0.
		ncol(ir)=2
		!write(*,*)' ir=',ir,' y1=',y1,' y2=',y2

		nonz=nonz+1
		aaa(nonz)=sm_s
		ncolrow(nonz) = nvel + i

		nonz=nonz+1
		aaa(nonz)=-sm_s
		ncolrow(nonz) = nvel + i+1
338		continue
	end do
end if

if(rg_p.gt.0.0001) then
	do i=1,nvel
		ir=ir+1
		dt(ir)=0.
		ncol(ir)=1

		nonz=nonz+1
		aaa(nonz) = rg_p
		ncolrow(nonz) = i
	end do
end if

if(rg_s.gt.0.0001) then
	do i=1,nvel
		ir=ir+1
		dt(ir)=0.
		ncol(ir)=1

		nonz=nonz+1
		aaa(nonz) = rg_s
		ncolrow(nonz) = nvel + i
	end do
end if




nz=nonz
nr=ir

do i=1,nr
	u(i)=dt(i)
end do


kount=0
do irw=1,nr
	!write(*,*)' irw=',irw,' dt=',u(irw)
	do ii=1,ncol(irw)
		kount=kount+1
		nn=ncolrow(kount)
		!write(*,*)' nn=',nn,' aaa=',aaa(kount)
		if(abs(u(irw)).gt.1.e10) then
			write(*,*)' Warning!!!'
			write(*,*)' dt=',u(irw)
			pause
		end if
		if(abs(aaa(kount)).gt.1.e10) then
			write(*,*)' Warning!!!'
			write(*,*)' aaa=',aaa(kount)
			pause
		end if
		if(nn.le.0.or.nn.gt.ncols) then
			write(*,*)' Warning!!!'
			write(*,*)' irw=',irw,' nn=',nn
			pause
		end if
	end do
end do
write(*,*)' N rows=',nr,' N columns=',ncols,' N nonzer=',nz
write(*,*)

!*******************************************************
!*******************************************************
!*******************************************************
call pstomo(nr,ncols,x,u,v,w,iter_lsqr,nz,aaa,ncolrow,ncol)
!*******************************************************
!*******************************************************
!*******************************************************

err=0
kount=0
avres=0.
avres0=0.
dtvel=0
dtsta=0
dtztr=0
!write(*,*)' nray=',nray
do irw=1,nray
	dt1=0
	do ii=1,ncol(irw)
		kount=kount+1
		nn=ncolrow(kount)
		dt1=dt1+aaa(kount)*x(nn)
		!write(*,*)' aaa(kount)*x(nn)',aaa(kount)*x(nn)
	end do
	!write(*,*)' dt(irw)=',dt(irw),' dt1=',dt1
	avres=avres+abs(dt(irw)-dt1)
	avres0=avres0+abs(dt(irw))
end do
avres=avres/nray
avres0=avres0/nray
reduct=((avres0-avres)/avres0)*100.

!write(*,*)'_____________________________________________________________'
write(*,*)' avres0=',avres0,' avres=',avres,' red=',reduct
!write(*,*)'_____________________________________________________________'

open(11,file='../../DATA/'//re//'/'//ar//'/RESULT/ref'//it1//'.dat')
write(11,*)0
iz0=0
if (hmod(1).lt.0) then
	vnew_p = vmodp(1) + x(1)
	vnew_s = vmods(1) + x(nvel+1)
	write(11,*)hmod(1),vnew_p,vnew_s,x(1),x(nvel+1)
	iz0=1
	vpnew(iz0)=vnew_p
	vsnew(iz0)=vnew_s
	hnew(iz0)=hmod(1)
end if

do iz=1,nvel
	zcur=(iz-1)*dz_par
	vold_p=vrefmod(zcur,1)
	vold_s=vrefmod(zcur,2)
	vnew_p = vold_p + x(iz)
	vnew_s = vold_s + x(nvel+iz)
	write(11,*)zcur,vnew_p,vnew_s,x(iz),x(nvel+iz)
	vpnew(iz+iz0)=vnew_p
	vsnew(iz+iz0)=vnew_s
	hnew(iz+iz0)=zcur
end do

do iref=1,nrefmod
	if (hmod(iref).le.zcur)cycle
	write(11,*)hmod(iref),vmodp(iref),vmods(iref)
	nvel=nvel+1
	vpnew(nvel+iz0)=vmodp(iref)
	vsnew(nvel+iz0)=vmods(iref)
	hnew(nvel+iz0)=hmod(iref)
end do

close(11)

open(21,file='../../FIG_files/1dmod/ref'//it1//'.bln')
write(21,*)nvel
do i=1,nvel+iz0
	write(21,*)vpnew(i),-hnew(i)
end do

write(21,*)nvel
do i=1,nvel
	write(21,*)vsnew(i),-hnew(i)
end do
close(21)


stop
end