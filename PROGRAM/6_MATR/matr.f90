character*8 ar,re,line
character*1 it,itold,ppss,atest,rm,gr

real xray(5000),yray(5000),zray(5000)
real resid(100),ztest(4),yprof(10,50)

integer iotper(5000),iotr4(4),nlev(2),nobr(2)

real  xtop(:,:,:), ztop(:,:,:),vtop(:,:,:)
integer popor(:,:,:),obr(:,:,:),kod_otr(:,:,:,:),notr(:,:),ntop(:,:)
real ornt(20),ylevel(:,:)


real zper(5000),votr(4),zotr(4),vaver(8)

real vertet(4,3),vtoptet(4)
real d00(1250),r00(1250)
real aa8(4,4),q8(4),a8(4)
real aa(4,4),q(4),a(4),curr(3)
integer iuz(8),nurr(8),muzel(1200)
real matr(:),matruzel(1200)
integer npar(50),nprof(10),ist1(100),nrps(2)


common/refmod/nrefmod,hmod(600),vmodp(600),vmods(600)
common/velmod/nnps,ypros,xpros,zpros,cfps
common/inipar/nzone,nsurf,smth,xlim1,xlim2,ylim1,ylim2,zlim1,zlim2
common/model/ar
common/itstep/itstep
common/inimodel/model_type
common/center/fi0,tet0

allocatable xtop,ztop,vtop,matr
allocatable popor,obr,kod_otr,notr,ntop,ylevel

one=1.e0
pi=asin(one)*2.e0
per=pi/180.e0

k_reject=1
rz=6371.


open(1,file='../../model.dat')
read(1,'(a8)')re		! code of the area
read(1,'(a8)')ar		! code of the area
read(1,*)iter		! code of the grid
read(1,*)igr		! code of the grid
close(1)
write(it,'(i1)')iter
write(gr,'(i1)')igr

write(*,*)' execution of matr'
write(*,*)' re=',re,' ar=',ar,' it=',it,' gr=',gr

!******************************************************************
open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/major_param.dat')
do i=1,10000
	read(1,'(a8)',end=573)line
	if(line.eq.'ORIENTAT') goto 574
end do
573 continue
write(*,*)' cannot find ORIENTATIONS in major_param.dat!!!'
pause
574 read(1,*)nornt
read(1,*)(ornt(i),i=1,nornt)
close(1)

orient=ornt(igr)
write(*,*)' orient=',orient
sinbase=sin(orient*per)
cosbase=cos(orient*per)

!pause
!******************************************************************
open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/major_param.dat')
do i=1,10000
	read(1,'(a8)',end=553)line
	if(line.eq.'AREA_CEN') goto 554
end do
553 continue
write(*,*)' cannot find AREA CENTER in major_param.dat!!!'
pause
554 read(1,*)fi0,tet0
write(*,*)fi0,tet0
close(1)

!******************************************************************
open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/major_param.dat')
do i=1,10000
	read(1,'(a8)',end=533)line
	if(line.eq.'GRID_PAR') goto 534
end do
533 continue
write(*,*)' cannot find GRID_PARAMETERS in major_param.dat!!!'
pause
534 continue
read(1,*)xlim1,xlim2,dxpl
read(1,*)ylim1,ylim2,dypl
read(1,*)zlim1,zlim2,dzpl
close(1)
!******************************************************************

nsurf=0
open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/ini_model.dat')
read(1,*,end=331)model_type
331 close(1)

if (model_type.eq.5) then
    call read_ini_model_3D(re,ar)
else if (model_type.eq.1) then
    call read_ini_model(re,ar)
end if

call read_3D_mod_v(re,ar,iter-1)

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
!write(*,*)' nrefmod=',nrefmod


!vvv=velocity(30.,20.,40.,1)


!**************************************************************
!*    Read initial parameters of grid

nmax=0
notmax=0

open(1,file='../../DATA/'//re//'/'//ar//'/TIMES/numray1.dat')
read(1,*) nrps(1),nrps(2)
close(1)


do iiips=1,2
	if(nrps(iiips).eq.0) cycle
	write(ppss,'(i1)')iiips
	open(1,file='../../DATA/'//re//'/'//ar//'/GRIDS/levinfo'//ppss//gr//'.dat')
	i=0
	722 i=i+1
		read(1,*,end=721)n,y
		goto 722
	721 nypl=i-1
	write(*,*)' nypl=',nypl
	nlev(iiips)=nypl
	close(1)

	open(1,file='../../DATA/'//re//'/'//ar//'/GRIDS/gr'//ppss//gr//'.dat')
	do n=1,nypl
		read(1,*)nt
		if(nt.gt.nmax) nmax=nt
		do i=1,nt
			read(1,*)x,z,l
		end do
	end do
	close(1)

	open(4,file='../../TMP/otr'//ppss//gr//'.dat')
	do nur=1,nypl
		read(4,*) no
		if(no.gt.notmax) notmax=no
		read(4,*) ((ll, i=1,2), j=1,no)
	end do
	close(4)

	open(1,file='../../DATA/'//re//'/'//ar//'/GRIDS/obr'//ppss//gr//'.dat')
	read(1,*) nobr(iiips)
	close(1)


end do
nymax=nlev(1)
if(nlev(2).gt.nymax) nymax=nlev(2)

ntmax=nobr(1)
if(nobr(2).gt.ntmax) ntmax=nobr(2)

write(*,*)' nymax=',nymax,' ntmax=',ntmax
write(*,*)' nmax=',nmax,' otr nmax=',notmax


allocate(ntop(2,nymax),ylevel(2,nymax))
allocate(xtop(2,nmax,nymax),ztop(2,nmax,nymax))
allocate(vtop(2,nmax,nymax),popor(2,nmax,nymax))
allocate(kod_otr(2,2,notmax,nymax),notr(2,nymax))
allocate(obr(2,ntmax,2),matr(ntmax))

do iiips=1,2
	if(nrps(iiips).eq.0) cycle
	write(ppss,'(i1)')iiips

	open(1,file='../../DATA/'//re//'/'//ar//'/GRIDS/levinfo'//ppss//gr//'.dat')
	do i=1,nlev(iiips)
		read(1,*)ntop(iiips,i),ylevel(iiips,i)
	end do
	close(1)

	open(1,file='../../DATA/'//re//'/'//ar//'/GRIDS/gr'//ppss//gr//'.dat')
	do n=1,nlev(iiips)
		read(1,*)nt
		ntop(iiips,n)=nt
		do i=1,ntop(iiips,n)
			read(1,*)xtop(iiips,i,n),ztop(iiips,i,n),popor(iiips,i,n)
			xx=xtop(iiips,i,n)
			yy=ylevel(iiips,n)
			zz=ztop(iiips,i,n)

			!if(zz.gt.130.and.popor(iiips,i,n).ne.0) then
				!write(*,*)popor(iiips,i,n),xx,yy,zz
				!pause
			!end if
			vtop(iiips,i,n)=velocity(xx,yy,zz,iiips)
		end do
	end do
	close(1)

	open(4,file='../../TMP/otr'//ppss//gr//'.dat')
	do nur=1,nlev(iiips)
		read(4,*) notr(iiips,nur)
		read(4,*) ((kod_otr(iiips,i,j,nur), i=1,2), j=1,notr(iiips,nur))
	end do

	close(4)


	open(1,file='../../DATA/'//re//'/'//ar//'/GRIDS/obr'//ppss//gr//'.dat')
	read(1,*) nobr(iiips)
	read(1,*)((obr(iiips,i,j),i=1,nobr(iiips)), j=1,2)
	close(1)
	write(*,*)' number of velocity parameters=',nobr(iiips)
end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

open(11,file='../../TMP/matr'//it//gr//'.dat',form='unformatted')

nzt=0
nray=0
nrp=0
nrs=0
nrr=0
nonzer=0
nonz_p=0
nonz_s=0


nr=0
open(1,file='../../DATA/'//re//'/'//ar//'/TIMES/rays_p'//it//'.dat',form='binary')
open(2,file='../../TMP/ray_paths_p_'//it//'.dat',form='binary')

728 continue

	read(1,end=729)xzt,yzt,zzt,nkrat,key_reloc
	nzt=nzt+1
    
	vzt1=velocity(xzt,yzt,zzt,1)
	vzt2=velocity(xzt,yzt,zzt,2)

	if(nkrat.eq.0) goto 728

	ik=0
	do ikr=1,nkrat
		read(1,end=729)ist,ips,tobs,tmod
		read(2)npr
		if(npr.eq.0)cycle
		!write(*,*) nzt,ikr
              	do i=1,npr
			read(2)xray(i),yray(i),zray(i)
			!write(*,*)xray(i),yray(i),zray(i)
		end do
                if (isnan(tmod)) cycle
		nr=nr+1
		!if(nr.lt.20080) cycle
		!do i=1,npr
		!	write(*,*)xray(i),yray(i),zray(i)
		!end do
		!write(*,*)nr,ist,ips,tobs,tmod
		s0=1/vzt1
		if(ips.ne.1) s0=1/vzt2
		x1=xray(1)
		y1=yray(1)
		z1=zray(1)
		x2=xray(2)
		y2=yray(2)
		z2=zray(2)

		hor=sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1))
		tot=sqrt(hor*hor+(z2-z1)*(z2-z1))

		cosbe=(x2-x1)/hor
		sinbe=(y2-y1)/hor
		!write(*,*)'x1=',x1,' y1=',y1,' z1=',z1
		!write(*,*)'x2=',x2,' y2=',y2,' z2=',z2
		!write(*,*)' dx=',x2-x1,' dy=',y2-y1,' dz=',z2-z1

		cosal=(z2-z1)/tot
		sinal=hor/tot

		dtdx=cosbe*sinal*s0
		dtdy=sinbe*sinal*s0
		dtdz=cosal*s0
		
		!write(*,*)'hor=',hor,'tot=',tot
		!pause

		!if(nr.lt.7) cycle

		res=tobs-tmod
		if(abs(res).gt.5) cycle

		matr=0.
		do ipt=2,npr
			x1=xray(ipt-1)
			y1=yray(ipt-1)
			z1=zray(ipt-1)
			x2=xray(ipt)
			y2=yray(ipt)
			z2=zray(ipt)
			!write(*,*)'x1=',x1,' y1=',y1,' z1=',z1
			!write(*,*)'x2=',x2,' y2=',y2,' z2=',z2
			ds=sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1)+(z2-z1)*(z2-z1))
			xmid=(x1+x2)/2.
			ymid=(y1+y2)/2.
			zmid=(z1+z2)/2.

			xxx=xmid*cosbase+ymid*sinbase
			yyy=-xmid*sinbase+ymid*cosbase
			zzz=zmid

!if(ipt.lt.41) cycle
!write(*,'(i5,3f10.3)')ipt,xxx,yyy,zzz
			do n=1,nlev(ips)-1
				yl1=ylevel(ips,n)
				yl2=ylevel(ips,n+1)
				if ((yyy-yl1)*(yyy-yl2).le.0.) exit
			end do
			nur=n
			nur1=nur
			nur2=nur+1


224			continue
			if((xxx-xlim1)*(xxx-xlim2).ge.0.) cycle
			if((yyy-ylim1)*(yyy-ylim2).ge.0.) cycle
			if((zzz-zlim1)*(zzz-zlim2).ge.0.) cycle


			iper=0
			do iotr=1,notr(ips,nur1)
				np1=kod_otr(ips,1,iotr,nur1)
				np2=kod_otr(ips,2,iotr,nur1)
				xp1=xtop(ips,np1,nur1)
				xp2=xtop(ips,np2,nur1)
				if(abs(xp1-xp2).lt.0.00001) cycle
				if((xp1-xxx)*(xp2-xxx).gt.0.) cycle
				!write(*,*)' xp1=',xp1,' xp2=',xp2,' xxx=', xxx
				zp1=ztop(ips,np1,nur1)
				zp2=ztop(ips,np2,nur1)
				!write(*,*)' zp1=',zp1,' zp2=',zp2
				iper=iper+1
				zper(iper)=zp1+((zp2-zp1)/(xp2-xp1))*(xxx-xp1)
				iotper(iper)=iotr
				!write(*,*)iper,' zp=',zper(iper),' iotr=',iotr
				!write(*,*)
			end do

			do ip=2,iper
				z1=zper(ip-1)
				z2=zper(ip)
				!write(*,*)ip,' z1=',z1,' z2=',z2,' zzz=',zzz
				if((z1-zzz)*(z2-zzz).le.0.) goto 710
			end do
								!cycle


								iper=0
								do iotr=1,notr(ips,nur1)
									np1=kod_otr(ips,1,iotr,nur1)
									np2=kod_otr(ips,2,iotr,nur1)
									xp1=xtop(ips,np1,nur1)
									xp2=xtop(ips,np2,nur1)
									!write(*,*)' xp1=',xp1,' xp2=',xp2
									if(abs(xp1-xp2).lt.0.00001) cycle
									if((xp1-xxx)*(xp2-xxx).gt.0.) cycle
									zp1=ztop(ips,np1,nur1)
									zp2=ztop(ips,np2,nur1)
									iper=iper+1
									zper(iper)=zp1+((zp2-zp1)/(xp2-xp1))*(xxx-xp1)
									iotper(iper)=iotr
									!write(*,*)' zp=',zper(iper),' iotr=',iotr
								end do


								do ip=1,iper
									write(*,*)' zper=',zper(ip)
								end do

								write(*,*)' xxx=',xxx,' yyy=',yyy,' zzz=',zzz
								write(*,*)' zlim1=',zlim1,' zlim2=',zlim2
								write(*,*)' out of the study volume'
								pause


710			continue
			iotr4(1)=iotper(ip-1)
			iotr4(2)=iotper(ip)
			!write(*,*)' iotr1=',iotr4(1),' iotr2=',iotr4(2)
			!write(*,*)' iper1=',iper


			iper=0
			do iotr=1,notr(ips,nur2)
				np1=kod_otr(ips,1,iotr,nur2)
				np2=kod_otr(ips,2,iotr,nur2)
				xp1=xtop(ips,np1,nur2)
				xp2=xtop(ips,np2,nur2)
				if(abs(xp1-xp2).lt.0.00001) cycle
				if((xp1-xxx)*(xp2-xxx).gt.0.) cycle
				zp1=ztop(ips,np1,nur2)
				zp2=ztop(ips,np2,nur2)
				!write(*,*)iotr,' x1=',xp1,xp2,' z1=',zp1,zp2
				iper=iper+1
				zper(iper)=zp1+((zp2-zp1)/(xp2-xp1))*(xxx-xp1)
				iotper(iper)=iotr
			end do


			do ip=2,iper
				z1=zper(ip-1)
				z2=zper(ip)
				if((z1-zzz)*(z2-zzz).le.0.) goto 711
			end do
											!cycle

											write(*,*)' iper=',iper
											do ip=1,iper
												write(*,*)' zper=',zper(ip)
											end do


											write(*,*)' xxx=',xxx,' yyy=',yyy,' zzz=',zzz
											write(*,*)' out of the study volume'
											pause
711			continue
			iotr4(3)=iotper(ip-1)
			iotr4(4)=iotper(ip)
			!write(*,*)' iotr3=',iotr4(3),' iotr4=',iotr4(4)

			iinf=0
			do iotr=1,4
				nnn=nur1
				if(iotr.gt.2) nnn=nur2
				do iside=1,2
					iuzz=kod_otr(ips,iside,iotr4(iotr),nnn)
					imatr=popor(ips,iuzz,nnn)
					if(imatr.eq.0) cycle
					if(iinf.ne.0) then
						do in=1,iinf
							if(iuz(in).eq.iuzz.and.nurr(in).eq.nnn) goto 712
						end do
					end if
					iinf=iinf+1
					!write(*,*)iinf,iuzz,nnn,imatr
					iuz(iinf)=iuzz
					nurr(iinf)=nnn
			!write(*,*)' iuz=',iuz(1),' nurr=',nurr(1)
					vaver(iinf)=vtop(ips,iuzz,nnn)
			!write(*,*)' iuz=',iuz(1),' nurr=',nurr(1)
712					continue
				end do
			end do

			do iotr=1,4
				nnn=nur1
				if(iotr.gt.2) nnn=nur2
				ii1=kod_otr(ips,1,iotr4(iotr),nnn)
				x1=xtop(ips,ii1,nnn)
				z1=ztop(ips,ii1,nnn)
				v1=vtop(ips,ii1,nnn)
				ii2=kod_otr(ips,2,iotr4(iotr),nnn)
				x2=xtop(ips,ii2,nnn)
				z2=ztop(ips,ii2,nnn)
				v2=vtop(ips,ii2,nnn)
				!write(*,*)' v1=',v1,' v2=',v2
				votr(iotr)=v1+((v2-v1)/(x2-x1))*(xxx-x1)
				zotr(iotr)=z1+((z2-z1)/(x2-x1))*(xxx-x1)
			end do
			z11=zotr(1)
			z21=zotr(2)
			z12=zotr(3)
			z22=zotr(4)
			v11=votr(1)
			v21=votr(2)
			v12=votr(3)
			v22=votr(4)
			!write(*,*)' v11=',v11,' v21=',v21
			!write(*,*)' v12=',v12,' v22=',v22
			vv1=v11+((v21-v11)/(z21-z11))*(zzz-z11)
			vv2=v12+((v22-v12)/(z22-z12))*(zzz-z12)
			!write(*,*)' vv1=',vv1,' vv2=',vv2
			y1=ylevel(ips,nur1)
			y2=ylevel(ips,nur2)
			v000=vv1+((vv2-vv1)/(y2-y1))*(yyy-y1)

!			write(*,*)' v000=',v000,' iinf=',iinf

			do itop=1,iinf
				!write(*,*)' iuz=',iuz(itop),' nurr=',nurr(itop)
				!pause
				iuz0=iuz(itop)
				nur0=nurr(itop)
				imatr=popor(ips,iuz0,nur0)
				if(imatr.eq.0) pause

				do iotr=1,4
					nnn=nur1
					if(iotr.gt.2) nnn=nur2
					!write(*,*)' iotr4(iotr)=',iotr4(iotr)
					!pause
					ii1=kod_otr(ips,1,iotr4(iotr),nnn)
					!write(*,*)' ii1=',ii1
					x1=xtop(ips,ii1,nnn)
					z1=ztop(ips,ii1,nnn)
					v1=vtop(ips,ii1,nnn)
					if(ii1.eq.iuz0.and.nnn.eq.nur0) then
						!write(*,*)' in node:',v1,v1*1.02
						v1=v1*1.02
					end if
					ii2=kod_otr(ips,2,iotr4(iotr),nnn)
					x2=xtop(ips,ii2,nnn)
					z2=ztop(ips,ii2,nnn)
					v2=vtop(ips,ii2,nnn)
					if(ii2.eq.iuz0.and.nnn.eq.nur0) then
						!write(*,*)' in node:',v2,v2*1.02
						v2=v2*1.02
					end if
					!write(*,*)' v1=',v1,' v2=',v2
					votr(iotr)=v1+((v2-v1)/(x2-x1))*(xxx-x1)
					zotr(iotr)=z1+((z2-z1)/(x2-x1))*(xxx-x1)
				end do
				!pause
				z11=zotr(1)
				z21=zotr(2)
				z12=zotr(3)
				z22=zotr(4)
				v11=votr(1)
				v21=votr(2)
				v12=votr(3)
				v22=votr(4)
				!write(*,*)' v11=',v11,' v21=',v21
				!write(*,*)' v12=',v12,' v22=',v22
				vv1=v11+((v21-v11)/(z21-z11))*(zzz-z11)
				vv2=v12+((v22-v12)/(z22-z12))*(zzz-z12)
				!write(*,*)' vv1=',vv1,' vv2=',vv2
				y1=ylevel(ips,nur1)
				y2=ylevel(ips,nur2)
				vfound=vv1+((vv2-vv1)/(y2-y1))*(yyy-y1)

				deltav=vfound-v000
				deltat=-deltav*ds/(v000*v000)
!write(*,*)' deltat=',deltat

				dmatr=deltat/(0.02*vaver(itop))
				matr(imatr)=matr(imatr)+dmatr
!write(*,*)' imatr=',imatr,' matr(imatr)=',matr(imatr)
			end do
		end do
		nuz=0
		do i=1,nobr(ips)
			if (abs(matr(i)).lt.1.e-10) cycle
			nuz=nuz+1
			muzel(nuz)=i
			matruzel(nuz)=matr(i)
!write(*,*)' muzel=',muzel(nuz),' m=',matruzel(nuz)
		end do
		nray=nray+1
		nonzer=nonzer+nuz
		if(ips.eq.1) then
			nrp=nrp+1
			nonz_p=nonz_p+nuz
		else
			nrs=nrs+1
			nonz_s=nonz_s+nuz
		end if

		if(mod(nray,1000).eq.0)write(*,'(4i6,f8.3)')nray,nzt,ips,nuz,res
		!if (nuz.eq.0) then
			!write(*,*) nuz,res,ist,ips,nzt
			!pause
		!end if
		!pause
		write(11) nuz,res,ist,ips,nzt
		write(11)dtdx,dtdy,dtdz
		!write(*,*)dtdx,dtdy,dtdz
		if(nuz.ne.0) then
			write(11) (muzel(i),matruzel(i),i=1,nuz)
!do i=1,nuz
	!write(*,*) muzel(i),matruzel(i)
!end do
		end if
	end do

	goto 728
729	close(1)
close(2)

close(11)

open(11,file='../../DATA/'//re//'/'//ar//'/TIMES/numbers'//it//gr//'.dat')
write(11,*)nray,nobr(1),nobr(2),nzt,nonzer
write(11,*)nrp,nonz_p
write(11,*)nrs,nonz_s
close(11)

write(*,*)' nray=',nray,' nzt=',nzt
write(*,*)' nobr=',nobr(1),nobr(2),' nonzer=',nonzer
write(*,*)' nrp=',nrp,' nonz_p=',nonz_p
write(*,*)' nrs=',nrs,' nonz_s=',nonz_s

stop
end
