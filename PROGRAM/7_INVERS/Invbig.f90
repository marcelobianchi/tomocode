character*2 itinv
character*8 ar,re,line
character*1 it,itold,ppss,rm,gr

real matruzel(10000)
integer muzel(10000)
real x,z,l
integer lixo

real ist_p(10000),ist_s(10000),dv_zn_p(3),dv_zn_s(3)

real ylev_p(200),ylev_s(200)
integer nt_p(200),npz_p(10),nt_s(200),npz_s(10),nrps(2)

allocatable  xtop_p(:,:), ztop_p(:,:)
allocatable  xtop_s(:,:), ztop_s(:,:)
allocatable kpop_p(:,:),kobr_p(:,:),kpz_p(:)
allocatable kpop_s(:,:),kobr_s(:,:),kpz_s(:)

allocatable dvp_it(:),dvs_it(:),dt_it(:)
allocatable x_ztr(:),y_ztr(:),z_ztr(:),t_ztr(:),p_sta(:),s_sta(:)

allocatable x(:),x0(:),u(:),v(:),w(:),aaa(:),dt(:),xmod(:)
allocatable ncolrow(:),ncol(:)

common/model/ar
common/velmod/nnps,ypros,xpros,zpros,cfps
common/inipar/nzone,nsurf,smth,xlim1,xlim2,ylim1,ylim2,zlim1,zlim2
common/itstep/itstep
common/refmod/nrefmod,hmod(600),vmodp(600),vmods(600)


pi=3.1415926
iter_max=1
it_curr=1
w_act=1


open(1,file='../../model.dat')
read(1,'(a8)')re		! code of the area
read(1,'(a8)')ar		! code of the area
read(1,*)iter		! code of the grid
read(1,*)igr		! code of the grid
close(1)
write(gr,'(i1)')igr
write(it,'(i1)')iter
write(itold,'(i1)')iter-1

write(*,*)' execution of invers'
write(*,*)' re=',re,' ar=',ar,' it=',it

!******************************************************************
open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/major_param.dat')
do i=1,10000
	read(1,'(a8)',end=573)line
	if(line.eq.'INVERSIO') goto 574
end do
573 continue
write(*,*)' cannot find INVERSION PARAMETERS in major_param.dat!!!'
call pause()
574 continue
read(1,*)iter_lsqr
read(1,*)wg_vel_p,wg_vel_s
read(1,*)sm_vel_p,sm_vel_s
read(1,*)rg_vel_p,rg_vel_s
read(1,*)
read(1,*)wg_st_p,wg_st_s
read(1,*)wzt_hor
read(1,*)wzt_ver
read(1,*)wzt_time
close(1)
!******************************************************************


open(1,file='../../DATA/'//re//'/'//ar//'/TIMES/numray1.dat')
read(1,*) nrps(1),nrps(2)
close(1)

! Read the values of the reference model
open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/refmod.dat')
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
write(*,*)' nrefmod=',nrefmod



open(1,file='../../DATA/'//re//'/'//ar//'/TIMES/numbers'//it//gr//'.dat')
read(1,*)nray,nobr_p,nobr_s,nztr,nonzer
read(1,*)nrp,nonz_p
read(1,*)nrs,nonz_s
close(1)

!write(*,*)' nztr=',nztr


allocate (x_ztr(nztr),y_ztr(nztr),z_ztr(nztr),t_ztr(nztr))
x_ztr=0
y_ztr=0
z_ztr=0
t_ztr=0

call read_3D_mod_v(re,ar,iter-1)


! Read the coordinates of the stations
open(2,file='../../DATA/'//re//'/'//ar//'/TIMES/stat_xy.dat')
i=0
3	i=i+1
	read(2,*,end=4)xstn,ystn,zstn
	goto 3
4	close(2)
nstat=i-1
write(*,*)' nst=',nstat


allocate (p_sta(nstat),s_sta(nstat))


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! BIANCHI
nvel_p = 0
nvel_s = 0

if(nrps(1).ne.0) then
	open(1,file='../../DATA/'//re//'/'//ar//'/GRIDS/obr1'//gr//'.dat')
	read(1,*) nvel_p
	allocate(dvp_it(nvel_p),kpz_p(nvel_p),kobr_p(nvel_p,2))
	read(1,*)((kobr_p(i,j),i=1,nvel_p), j=1,2)
	dvp_it=0
	close(1)
end if

if(nrps(2).ne.0) then
	open(1,file='../../DATA/'//re//'/'//ar//'/GRIDS/obr2'//gr//'.dat')
	read(1,*) nvel_s
	allocate(dvs_it(nvel_s),kpz_s(nvel_s),kobr_s(nvel_s,2))
	read(1,*)((kobr_s(i,j),i=1,nvel_s), j=1,2)
	dvs_it=0
	close(1)
end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

if(nrps(1).ne.0) then
	open(1,file='../../DATA/'//re//'/'//ar//'/GRIDS/levinfo1'//gr//'.dat')
	i=0
	722 i=i+1
		read(1,*,end=721)nt,ylev_p(i)
		goto 722
	721 ny_p=i-1
	close(1)
	write(*,*)' ny_p=',ny_p
end if

if(nrps(2).ne.0) then
	open(1,file='../../DATA/'//re//'/'//ar//'/GRIDS/levinfo2'//gr//'.dat')
	i=0
	723 i=i+1
		read(1,*,end=724)nt,ylev_p(i)
		goto 723
	724 ny_s=i-1
	close(1)
	!write(*,*)' ny_s=',ny_s
end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

if(nrps(1).ne.0) then
	open(1,file='../../DATA/'//re//'/'//ar//'/GRIDS/gr1'//gr//'.dat')
	nmax=0
	do n=1,ny_p
		read(1,*)nt
		if(nt.gt.nmax) nmax=nt
		do i=1,nt
			read(1,*)x,z,l
		end do
	end do
	close(1)

	allocate(xtop_p(nmax,ny_p),ztop_p(nmax,ny_p),kpop_p(nmax,ny_p))

	open(1,file='../../DATA/'//re//'/'//ar//'/GRIDS/gr1'//gr//'.dat')
	npz_p=0
	do n=1,ny_p
		read(1,*)nt
	        !write(*,*)n,' ntop=',nt
		nt_p(n)=nt
		do i=1,nt_p(n)
			read(1,*)xtop_p(i,n),ztop_p(i,n),kpop_p(i,n),izone
	!if(it_curr.ne.1)write(*,*)xtop(i,n),ztop(i,n),popor(i,n),izone
			if(kpop_p(i,n).eq.0) cycle
			if(izone.ne.0) npz_p(izone)=npz_p(izone)+1
			imatr=kpop_p(i,n)
			kpz_p(imatr)=izone
		end do
	end do
	close(1)
	isum=0
	do i=1,nzone
		isum=isum+npz_p(i)
	end do
end if

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

if(nrps(2).ne.0) then
	open(1,file='../../DATA/'//re//'/'//ar//'/GRIDS/gr2'//gr//'.dat')
	nmax=0
	do n=1,ny_s
		read(1,*)nt
		if(nt.gt.nmax) nmax=nt
		do i=1,nt
			read(1,*)x,z,l,lixo,lixo
		end do
	end do
	close(1)

	allocate(xtop_s(nmax,ny_s),ztop_s(nmax,ny_s),kpop_s(nmax,ny_s))

	open(1,file='../../DATA/'//re//'/'//ar//'/GRIDS/gr2'//gr//'.dat')
	npz_s=0
	do n=1,ny_s
		read(1,*)nt
	!write(*,*)n,' ntop=',nt
		nt_s(n)=nt
		do i=1,nt_s(n)
			read(1,*)xtop_s(i,n),ztop_s(i,n),kpop_s(i,n),izone
	!if(it_curr.ne.1)write(*,*)xtop(i,n),ztop(i,n),popor(i,n),izone
			if(kpop_s(i,n).eq.0) cycle
			if(izone.ne.0) npz_s(izone)=npz_s(izone)+1
			imatr=kpop_s(i,n)
			kpz_s(imatr)=izone
		end do
	end do
	close(1)
	isum=0
	do i=1,nzone
		isum=isum+npz_s(i)
	end do
end if

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 

444	continue

!write(*,*)
!write(*,*)'* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *'
!write(*,*)'~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
!write(*,*)' I T E R A T I O N:',it_curr
!write(*,*)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

nonz=0
ir=0
avdt=0
totmat=0
nst_p=0
nst_p=0

open(3,file='../../TMP/matr'//it//gr//'.dat',form='unformatted')

95	read(3,end=96) nuz,resid,ist,ips,izt
	!write(*,*) nuz,resid,ist,ips,izt
	read(3)dtdx,dtdy,dtdz
	if(nuz.ne.0)read(3) (muzel(ii),matruzel(ii),ii=1,nuz)

	ir=ir+1

	nonz = nonz + nuz 
	if(ist.eq.0) goto 95

	nonz = nonz + 5

	if(ips.eq.1)then
		if(nst_p.ne.0) then
			do i=1,nst_p
				if(ist_p(i).eq.ist) goto 298
			end do
		end if
		nst_p=nst_p+1
		ist_p(nst_p)=ist
	else
		if(nst_s.ne.0) then
			do i=1,nst_s
				if(ist_s(i).eq.ist) goto 298
			end do
		end if
		nst_s = nst_s + 1
		ist_s(nst_s) = ist
	end if

298	continue
	!if(ir.eq.35375)write(*,*)' ir=',ir,' nonz=',nonz

	goto 95
96  close(3)

!write(*,*)' Main matrix: nonz=',nonz,' ir=',ir

if(it_curr.eq.1) then
	allocate (dt_it(ir))
	write(*,*)' Number of rays =',nray,ir,' nzt=',nztr
	write(*,*)' nst_p=',nst_p,' nst_s=',nst_s
	dt_it=0
	p_sta=0
	s_sta=0
end if


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

nc = nvel_p + nvel_s + nst_p + nst_s + nztr*4

nonzer=nonz
!write(*,*)nvel_p,nvel_s,nst_p,nst_s,nztr
!write(*,*)' number of columns: =',nc


np1=nvel_p
np2=np1+nvel_s
np3=np2+nst_p
np4=np3+nst_s
write(*,*)' np1=',np1,' np2=',np2,' np3=',np3

nrows=nray

if(nrps(1).ne.0) then
	open(1,file='../../TMP/num_sos1'//gr//'.dat')
	read(1,*)nsos_p
	!write(*,*)' nsos_p=',nsos_p
	close(1)
end if

if(nrps(2).ne.0) then
	open(1,file='../../TMP/num_sos2'//gr//'.dat')
	read(1,*)nsos_s
	close(1)
end if

if(sm_vel_p.gt.0.0001.and.nrps(1).ne.0) then
	nonzer = nonzer + nsos_p*2
	nrows = nrows + nsos_p
end if

if(sm_vel_s.gt.0.0001.and.nrps(2).ne.0) then
	nonzer = nonzer + nsos_s*2
	nrows = nrows + nsos_s
end if

if(rg_vel_p.gt.0.0001.and.nrps(1).ne.0) then
	nonzer=nonzer+nvel_p
	nrows=nrows+nvel_p
end if

if(rg_vel_s.gt.0.0001.and.nrps(2).ne.0) then
	nonzer=nonzer+nvel_s
	nrows=nrows+nvel_s
end if

write(*,*)' Preliminary values: N rows=',nrows,' N nonzer=',nonzer
write(*,*)
!call pause()

if(it_curr.eq.1) then
	allocate(xmod(nc),x(nc),x0(nc),v(nc),w(nc))
	allocate(ncol(nrows),dt(nrows),u(nrows))
	allocate(aaa(nonzer),ncolrow(nonzer))
end if

!write(*,*)'ok'
ir=0
nonz=0  

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
open(3,file='../../TMP/matr'//it//gr//'.dat',form='unformatted')
97	read(3,end=98) nuz,resid,ist,ips,izt
	read(3)dtdx,dtdy,dtdz
	if(nuz.ne.0)read(3) (muzel(ii),matruzel(ii),ii=1,nuz)
	ir=ir+1


	w_data=1
	if(ist.eq.0) w_data = w_act

	dt(ir) = resid * w_data - dt_it(ir)
	do ii=1,nuz
		mmm=muzel(ii)
		nnp=0
		if(ips.eq.2) nnp=np1
		add = wg_vel_p
		if(ips.eq.2) add = wg_vel_s
		nonz=nonz+1
		aaa(nonz) = matruzel(ii) * add * w_data
		!if (mmm.eq.4544)write(*,*)mmm,aaa(nonz)
		ncolrow(nonz)=nnp+mmm
	end do
	!write(*,*)' dt=',dt(ir)

	ncol(ir) = nuz 

	if(ist.eq.0) goto 97

	ncol(ir) = ncol(ir) + 4 + 1 

	!call pause()
	!if(mod(ir,100).eq.0)write(*,*)' ir=',ir,' nonz=',nonz
	if(ips.eq.1) then
		do i=1,nst_p
			if(ist_p(i).ne.ist) cycle
			nonz=nonz+1
			if(nonz.ge.nonzer) call pause()
			aaa(nonz)=wg_st_p
			ncolrow(nonz)=np2+i
			exit
		end do
	else
		do i=1,nst_s
			if(ist_s(i).ne.ist) cycle
			nonz=nonz+1
			if(nonz.ge.nonzer) call pause()
			aaa(nonz)=wg_st_s
			ncolrow(nonz)=np3+i
			exit
		end do
	end if

	nonz=nonz+1
	aaa(nonz)=dtdx*wzt_hor	
	ncolrow(nonz)=np4 + (izt-1)*4 + 1
!	write(*,*)' ztr:',nonz,ncolrow(nonz),aaa(nonz)
	nonz=nonz+1
	aaa(nonz)=dtdy*wzt_hor	
	ncolrow(nonz)=np4 + (izt-1)*4 + 2
!	write(*,*)' ztr:',nonz,ncolrow(nonz),aaa(nonz)
	nonz=nonz+1
	aaa(nonz)=dtdz*wzt_ver	
	ncolrow(nonz)=np4 + (izt-1)*4 + 3
!	write(*,*)' ztr:',nonz,ncolrow(nonz),aaa(nonz)
	nonz=nonz+1
	aaa(nonz)=wzt_time	
	ncolrow(nonz)=np4 + (izt-1)*4 + 4
!	write(*,*)' ztr:',nonz,ncolrow(nonz),aaa(nonz)
	goto 97
98    continue
close(3)
!write(*,*)' Main matrix: nonz=',nonz,' ir=',ir


! Read the elements responsible for the neigbour nodes, P-model
if(sm_vel_p.gt.0.0001.and.nrps(1).ne.0) then
	open(2,file='../../TMP/sosedi1'//gr//'.dat',form='unformatted')
	do isos=1,nsos_p
		read(2)ipar1,ipar2,dist
! P-model:
		ir=ir+1
		dt(ir)=0.
		ncol(ir)=2

		smth=sm_vel_p

		nonz=nonz+1
		aaa(nonz)=smth
		ncolrow(nonz) = ipar1

		nonz=nonz+1
		aaa(nonz)=-smth
		ncolrow(nonz) = ipar2

	end do
	close(2)
end if

! Read the elements responsible for the neigbour nodes, P-model
if(sm_vel_s.gt.0.0001.and.nrps(2).ne.0) then
	open(2,file='../../TMP/sosedi2'//gr//'.dat',form='unformatted')
	do isos=1,nsos_s
		read(2)ipar1,ipar2,dist
! P-model:
		ir=ir+1
		dt(ir)=0.
		ncol(ir)=2

		nonz=nonz+1
		aaa(nonz)=sm_vel_s
		ncolrow(nonz) = np1 + ipar1

		nonz=nonz+1
		aaa(nonz)=-sm_vel_s
		ncolrow(nonz) = np1 + ipar2

	end do
	close(2)
end if



!write(*,*)' After Smoothing : ir=',ir,' nonz=',nonz

if(rg_vel_p.gt.0.0001.and.nrps(1).ne.0) then
	do i=1,nvel_p 
		ir=ir+1
		dt(ir)=0.
		ncol(ir)=1
		nonz=nonz+1
		aaa(nonz)=rg_vel_p
		ncolrow(nonz)= i
	end do
end if

if(rg_vel_s.gt.0.0001.and.nrps(2).ne.0) then
	do i=1,nvel_s 
		ir=ir+1
		dt(ir)=0.
		ncol(ir)=1
		nonz=nonz+1
		aaa(nonz)=rg_vel_s
		ncolrow(nonz)= np1 + i
	end do
end if

!write(*,*)' After Regularization: ir=',ir,' nonz=',nonz

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
			call pause()
		end if
		if(abs(aaa(kount)).gt.1.e10) then
			write(*,*)' Warning!!!'
			write(*,*)' aaa=',aaa(kount)
			call pause()
		end if
		if(nn.le.0.or.nn.gt.nc) then
			write(*,*)' Warning!!!'
			write(*,*)' irw=',irw,' nn=',nn
			call pause()
		end if
	end do
end do
write(*,*)' N rows=',nr,' N columns=',nc,' N nonzer=',nz
write(*,*)

!*******************************************************
!*******************************************************
!*******************************************************
call pstomo(nr,nc,x,u,v,w,iter_lsqr,nz,aaa,ncolrow,ncol)
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
write(*,*)' nray=',nray
do irw=1,nray
	dt1=0
	do ii=1,ncol(irw)
		kount=kount+1
		nn=ncolrow(kount)
		dt1=dt1+aaa(kount)*x(nn)
	end do
	if(irw.le.nray)dt_it(irw)=dt_it(irw)+dt1
	avres=avres+abs(dt(irw)-dt1)
	avres0=avres0+abs(dt(irw))
end do
avres=avres/nray
avres0=avres0/nray
reduct=((avres0-avres)/avres0)*100.

write(*,*)'_____________________________________________________________'
write(*,*)' avres0=',avres0,' avres=',avres,' red=',reduct
write(*,*)'_____________________________________________________________'


if(nrps(1).ne.0) then 
	avdvp=0
	dv_zn_p=0
	open(11,file='../../DATA/'//re//'/'//ar//'/RESULT/vel_p_'//it//gr//'.dat')
	if(iter.gt.1) open(1,file='../../DATA/'//re//'/'//ar//'/RESULT/vel_p_'//itold//gr//'.dat')
	do i=1,nvel_p

		if(iter.gt.1) then
			read(1,*)dvp_old,vvp,izone
		else
			dvp_old=0
			i1=kobr_p(i,1)
			i2=kobr_p(i,2)
			!write(*,*)' i1=',i1,' i2=',i2
			if(i1.eq.0) cycle
			xx=xtop_p(i1,i2)
			yy=ylev_p(i2)
			zz=ztop_p(i1,i2)

			izone=kpz_p(i)
			vvp=velocity(xx,yy,zz,1)
			!write(*,*)' xx=',xx,' yy=',yy,' zz=',zz
			!write(*,*)' vvp=',vvp
		end if

		dvp = x(i) * wg_vel_p

		dvp_it(i) = dvp_it(i) + dvp

		dvp_new = dvp_old + dvp_it(i)

		write(11,*)dvp_new,vvp,izone

		avdvp=avdvp+100*abs(dvp)/vvp
		dv_zn_p(izone) = dv_zn_p(izone) + 100*abs(dvp)/vvp
	end do
	close(11)
	if(iter.gt.1) close(1)
	avdvp=avdvp/nvel_p
end if

write(*,*)' nrps=',nrps(1),nrps(2)

if(nrps(2).ne.0) then 
	avdvs=0
	dv_zn_s=0
	open(11,file='../../DATA/'//re//'/'//ar//'/RESULT/vel_s_'//it//gr//'.dat')
	if(iter.gt.1) open(1,file='../../DATA/'//re//'/'//ar//'/RESULT/vel_s_'//itold//gr//'.dat')
	do i=1,nvel_s

		if(iter.gt.1) then
			read(1,*)dvs_old,vvs,izone
		else
			dvs_old=0
			i1=kobr_s(i,1)
			i2=kobr_s(i,2)
			!write(*,*)' i1=',i1,' i2=',i2
			if(i1.eq.0) cycle
			xx=xtop_s(i1,i2)
			yy=ylev_s(i2)
			zz=ztop_s(i1,i2)

			!write(*,*)' xx=',xx,' yy=',yy,' zz=',zz
			izone=kpz_s(i)
			vvs=velocity(xx,yy,zz,2)
		end if

		dvs = x(np1+i) * wg_vel_s

		dvs_it(i) = dvs_it(i) + dvs

		dvs_new = dvs_old + dvs_it(i)

		write(11,*)dvs_new,vvs,izone

		avdvs=avdvs+100*abs(dvs)/vvs
		dv_zn_s(izone) = dv_zn_s(izone) + 100*abs(dvs)/vvs
	end do
	close(11)
	if(iter.gt.1) close(1)
	avdvs=avdvs/nvel_s
	avdv=0
	do i=1,nst_s
		ii=np3+i
		avdv=avdv+abs(x(ii)*wg_st_s)
	end do
	avstat_s=avdv/nst_s
	open(12,file='../../DATA/'//re//'/'//ar//'/RESULT/stcor_s_'//it//gr//'.dat')
	do ist=1,nstat
		s_sta(ist)=0
		do i=1,nst_s
			if(ist_s(i).ne.ist) cycle
			ii=np3+i
			s_sta(ist)=x(ii)*wg_st_s
			exit
		end do
		write(12,*)s_sta(ist)
	end do
	close(12)
end if

write(*,*)' total avdv_p=',avdvp,' avdv_s=',avdvs


avdv=0
do i=1,nst_p
	ii=np2+i
	avdv=avdv+abs(x(ii)*wg_st_p)
end do
avstat_p=avdv/nst_p


open(12,file='../../DATA/'//re//'/'//ar//'/RESULT/stcor_p_'//it//gr//'.dat')
do ist=1,nstat
	p_sta(ist)=0
	do i=1,nst_p
		if(ist_p(i).ne.ist) cycle
		ii=np2+i
		p_sta(ist)=x(ii)*wg_st_p
	end do
	write(12,*)p_sta(ist)
end do
close(12)

write(*,*)' avstat_p=',avstat_p,' avstat_s=',avstat_s

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


open(12,file='../../DATA/'//re//'/'//ar//'/RESULT/ztcor_'//it//gr//'.dat')
av1=0
av2=0
av3=0
do i=1,nztr
	ii=np4+(i-1)*4
	x_ztr(i)=x_ztr(i)+x(ii+1)*wzt_hor
	y_ztr(i)=y_ztr(i)+x(ii+2)*wzt_hor
	z_ztr(i)=z_ztr(i)+x(ii+3)*wzt_ver
	t_ztr(i)=t_ztr(i)+x(ii+4)*wzt_time
	write(12,*)x_ztr(i),y_ztr(i),z_ztr(i),t_ztr(i)
	av1=av1+sqrt(x_ztr(i)*x_ztr(i)+y_ztr(i)*y_ztr(i))
	av2=av2+abs(z_ztr(i))
	av3=av3+abs(t_ztr(i))
end do
av1=av1/nztr
av2=av2/nztr
av3=av3/nztr
write(*,*)' source corrections:'
write(*,*)' av1=',av1,' av2=',av2,' av3=',av3
close(12)

it_curr = it_curr + 1
if(it_curr.le.iter_max) goto 444


STOP
end

