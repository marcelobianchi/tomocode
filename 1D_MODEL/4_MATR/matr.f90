character*8 ar,re,line
character*1 it0,it1
real xstat(9000),ystat(9000),zstat(9000)
real tall(50),hall(50),aall(50)
real amat1(10000),amat_line(10000)
integer kmat_line(10000)

common/pi/pi,per
!common /ray_path/npray,xray(5000),yray(5000),zray(5000)
common/ray/ npray,xray(10000),yray(10000),zray(10000)
common/grid/zgrmax,dzlay,dsmin
common/refmod/nrefmod,hmod(600),vmodp(600),vmods(600)
common/ray_param/ds_ini1,ds_ini2,ds_bend,ds_bend1,dist0,val_bend_min
common/center/fi0,tet0

ds_ini1=10.
ds_ini2=0.5
dist0=300.


one=1.d0
pi=asin(one)*2.d0
per=pi/180.d0

dsmin=1
dzlay=1
zgrmax=300


open(1,file='../../model.dat')
read(1,'(a8)')re		! code of the area
read(1,'(a8)')ar		! code of the area
read(1,*)iter
close(1)
write(it0,'(i1)')iter-1
write(it1,'(i1)')iter
write(*,*)' area=',re,' model=',ar

open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/1Dmod_PARAM.DAT')
read(1,*)
read(1,*)zmin,zmax,dzstep,nev
read(1,*)dsmin,dzlay,zgrmax
read(1,*)dz_par
close(1)

open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/major_param.dat')
do i=1,10000
	read(1,'(a8)',end=553)line
	if(line.eq.'AREA_CEN') goto 554
end do
553 continue
write(*,*)' cannot find AREA CENTER in major_param.dat!!!'
call pause()

554 read(1,*)fi0,tet0
close(1)



iter=1
call prepare_ref(re,ar)
call read_3D_mod_v(re,ar,iter-1)
call read_z_lim(re,ar)


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
write(*,*)' nrefmod=',nrefmod



! Read the coordinates of the stations
open(1,file='../../DATA/'//re//'/'//ar//'/TIMES/stat_xy.dat')
nst=0
33	read(1,*,end=44)xst,yst,zst
	nst=nst+1
	xstat(nst)=xst
	ystat(nst)=yst
	zstat(nst)=zst
	goto 33
44	close(1)
write(*,*)' nst=',nst

open(11,file='../../TMP/matr_1d.dat',form='unformatted')

open(1,file='../../DATA/'//re//'/'//ar//'/TIMES/rays'//it1//'.dat',form='unformatted')

nzt=0
nray=0
npar_max=0
nonz_tot=0
992	continue
	read(1,end=991)xzt,yzt,zzt,nkrat
	nzt=nzt+1
	do ikr=1,nkrat
		read(1)ist,ips,tobs,tref
		res=tobs-tref
		nray=nray+1
		xst=xstat(ist)
		yst=ystat(ist)
		zst=zstat(ist)
		dshor=sqrt((xst-xzt)*(xst-xzt)+(yst-yzt)*(yst-yzt))
		distance=sqrt(dshor*dshor+zzt*zzt)


		!call refmod_all(dshor,zzt,ips, nall,tall,hall,aall)
		call ref_time(dshor,zzt,ips, tttt,hhhh,alfa)
		!write(*,*)' zzt=',zzt,' dis=',dshor,' ips=',ips
		!write(*,*)' t=',(tall(i),i=1,nall)


	! Select the first arrival
		!tmin=999999
		!do ii=1,nall
		!	if(tall(ii).gt.tmin) cycle
		!	tmin=tall(ii)
		!	imin=ii
		!end do
		!hhhh=hall(imin)
		!alfa=aall(imin)
		!tttt=tmin
		!write(*,*)' aaaa=',alfa,' tttt=',tttt

		cosb=(xzt-xst)/dshor
		sinb=(yzt-yst)/dshor
		sina=sin(alfa*per)
		cosa=cos(alfa*per)
		vzt=vrefmod(zzt,ips)
		dtdd=sina/vzt
		dtdz=-cosa/vzt
		dtdx=dtdd*cosb
		dtdy=dtdd*sinb




		!call ray_xyz(xzt,yzt,zzt, xst,yst, alfa,ips, hmax)
		call trace_1D(xzt,yzt,zzt,xst,yst,zst,ips, tout)



		!write(*,*)' hmax=',hmax

		!do il=1,npray
		!	write(*,*)xray(il),yray(il),zray(il)
		!end do
		!call pause()
		amat1=0
		do il=2,npray
			!write(*,*)' il=',il
			xx1=xray(il-1)
			yy1=yray(il-1)
			zz1=zray(il-1)
			xx2=xray(il)
			yy2=yray(il)
			zz2=zray(il)
			dss=sqrt((xx1-xx2)*(xx1-xx2)+(yy1-yy2)*(yy1-yy2)+(zz1-zz2)*(zz1-zz2))
			xmid=(xx1+xx2)/2
			ymid=(yy1+yy2)/2
			zmid=(zz1+zz2)/2

			vmid=vrefmod(zmid,ips)

			do i_par=1,1000
				z1=(i_par-1)*dz_par
				z2=i_par*dz_par
				if((zmid-z1)*(zmid-z2).le.0) exit
			end do
			
			!if (z1.gt.zmax) cycle

			dv_on_ray_1= (zmid-z2)/(z1-z2)
			dmatr = - dv_on_ray_1 * dss / vmid**2 
			amat1(i_par)=amat1(i_par)+dmatr

			dv_on_ray_2= (zmid-z1)/(z2-z1)
			dmatr = - dv_on_ray_2 * dss / vmid**2 
			amat1(i_par+1)=amat1(i_par+1)+dmatr

		end do



		nonz=0
		do i=1,1000
			if(abs(amat1(i)).lt.1.e-15) cycle
			nonz=nonz+1
			amat_line(nonz)=amat1(i)
			kmat_line(nonz)=i
			if(i.gt.npar_max) npar_max=i
			!write(*,*)nonz,i,amat1(i)
		end do

		nonz_tot = nonz_tot + nonz

		write(11)nonz,res,ips
		write(11)nzt,dtdx,dtdy,dtdz
		do i=1,nonz
			write(11)amat_line(i),kmat_line(i)
			!write(*,*)amat_line(i),kmat_line(i)
		end do

	end do
	goto 992
991 close(1)
close(11)

write(*,*)' npar_max=',npar_max


write(*,*)' nzt=',nzt,' nray=',nray,' nonz_tot=',nonz_tot

open(11,file='../../DATA/'//re//'/'//ar//'/TIMES/numb_1d.dat')
write(11,*)nray,nonz_tot,npar_max,nzt
close(11)

stop
end
