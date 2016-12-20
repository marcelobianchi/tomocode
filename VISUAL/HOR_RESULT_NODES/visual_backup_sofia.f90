! HORIZONTAL !!!
! NODES !!!!!!!

character*4 dsaa/'DSAA'/
character*8 ar,re,line
character*2 lv
character*1 ps ,rm,it
allocatable dvan(:,:),vvv(:,:),v1tmp(:,:),v2tmp(:,:),dvapr(:,:)
real hlev(20),avlev(20),avlevP(20),avlevS(20)
integer nrps(2)
real fzzt(10000,200),tzzt(10000,200),zzzt(10000,200)
integer nzzt(200)
real far(200),tar(200) !,dv_aprio


common/pi/pi,per
common/center/fi0,tet0
common/refmod/nrefmod,hmod(600),vmodp(600),vmods(600)

one=1.e0
pi=asin(one)*2.e0
per=pi/180.e0
rz=6371.

w_limit=0.2

igr=1


open(1,file='../../VISUAL/area.dat')
read(1,'(a8)')re
read(1,'(a8)')ar
read(1,*) iter
read(1,*) ngr1,ngr2
read(1,*) add_perc
read(1,*) kod_av_bias
read(1,*) kod_apriori
read(1,*) 
ind_srce=0
read(1,*,end=825) ind_srce
ind_cut=0
read(1,*,end=825) ind_cut
825 close(1)

write(*,*)' re=',re,' ar : ',ar
write(it,'(i1)')iter

open(66,file='../../FIG_FILES/HOR/refvelocities.dat')
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

open(2,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/sethor.dat')
read(2,*) nlev  
read(2,*) (hlev(i),i=1,nlev)  
read(2,*) fmap1,fmap2,dfmap,tmap1,tmap2,dtmap  
read(2,*) smaxx
read(2,*) ismth
close(2)

if (ind_cut.eq.1) then 
	nnod=1
	open(10,file='../../DATA/'//re//'/INIDATA/good_area.bln')
	read(10,*,err=20,end=20) nnod
		do i=1,nnod
			read(10,*)far(i),tar(i)
		end do
		far(nnod+1)=far(1)
		tar(nnod+1)=tar(1)
	20	close(10)
end if


if(kod_apriori.eq.1) then
    open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/ini_model.dat')
    read(1,*)model_type
    close(1)
    if (model_type.eq.5) then
        call read_ini_model_3D(re,ar)
    else
	call read_ini_model(re,ar)
    end if
end if

rsmth=ismth+0.5
nfmap=int_best((fmap2-fmap1)/dfmap+1.)
ntmap=int_best((tmap2-tmap1)/dtmap+1.)
write(*,*)' nfmap=',nfmap,' ntmap=',ntmap
allocate(dvan(nfmap,ntmap),vvv(nfmap,ntmap),v1tmp(nfmap,ntmap),v2tmp(nfmap,ntmap),dvapr(nfmap,ntmap))


!call read_3D_mod_v(ar,iter-1)

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



open(14,file='../../FIG_FILES/1DMOD/'//re//ar//'.bln')
write(14,*)nrefmod
do i=1,nrefmod
	write(14,*)vmodp(i),-hmod(i)
end do
write(14,*)nrefmod
do i=1,nrefmod
	write(14,*)vmods(i),-hmod(i)
end do
close(14)

open(1,file='../../DATA/'//re//'/'//ar//'/TIMES/numray1.dat')
read(1,*) nrps(1),nrps(2)
close(1)


if(ind_srce.ne.0) then
	nzzt=0
	nzt=0
	open(1,file='../../DATA/'//re//'/'//ar//'/TIMES/rays_p'//it//'.dat',form='unformatted')
	872	read(1,end=871)xzt,yzt,zzt,nkrat,key_reloc
	do ikr=1,nkrat
		read(1,end=871)i1,i2,a1,a2
	end do
	call decsf(xzt,yzt,0.,fi0,tet0,fzt,tzt,h)
	nzt=nzt+1
	do ilev=1,nlev
		if(ilev.eq.1) then
			z1=-10
			z2=(hlev(1)+hlev(2))/2
		else if(ilev.eq.nlev) then
			z1=(hlev(nlev-1)+hlev(nlev))/2
			z2=hlev(nlev) + (hlev(nlev)-hlev(nlev-1))/2
		else 
			z1=(hlev(ilev-1)+hlev(ilev))/2
			z2=(hlev(ilev+1)+hlev(ilev))/2
		end if
		if((zzt-z1)*(zzt-z2).le.0) goto 995
	end do
	goto 872

995 continue
	!write(*,*)' zzt=',zzt,' ilev=',ilev,' z1=',z1,' z2=',z2
	nzzt(ilev)=nzzt(ilev)+1
	fzzt(nzzt(ilev),ilev)=fzt
	tzzt(nzzt(ilev),ilev)=tzt
	zzzt(nzzt(ilev),ilev)=zzt

	goto 872
871 close(1)
	DO ilev=1,nlev
		write(lv,'(i2)')ilev
		open(11,file='../../FIG_FILES/HOR/ztr'//lv//'.dat')
		write(*,*)' ilev=',ilev,' nzzt=',nzzt(ilev)
		do izzt=1,nzzt(ilev)
			write(11,*)fzzt(izzt,ilev),tzzt(izzt,ilev),zzzt(izzt,ilev)
		end do
		close(11)
	end do
end if

avlevP=0
avlevS=0

do ips=1,2
	avlev=0
	if(nrps(ips).eq.0) cycle
	write(ps,'(i1)')ips
	avertot=0
	navertot=0
	DO ilev=1,nlev
		dvan=0
		vvv=0
		zzz=hlev(ilev)
		v0=vrefmod(zzz,ips)
		!~zzz=15
		write(lv,'(i2)')ilev
		write(*,*)' ilev=',ilev,' zzz=',zzz
		write(66,*) ilev,zzz,v0
		do igr=ngr1,ngr2
			call prepare_model_v(re,ar,ips,iter,igr)
			do itet=1,ntmap
				ttt=(itet-1)*dtmap+tmap1+tet0
				!write(*,*)' itet=',itet,' ttt=',ttt
				!ttt=-7.45
				do ifi=1,nfmap
					fff=(ifi-1)*dfmap+fmap1+fi0
					!fff=fi0+1
					!fff=110.87
					dv=0
					www=0
					call dv_1_grid_v(fff,ttt,zzz,smaxx, dv,www)
					!write(*,*)' dv=',dv,' www=',www
					if (ind_cut.eq.1) then
						if (nnod.gt.2) then	
							nn=0
							ntot=0
							do i=2,nnod+1
								if ( (fff-far(i))*(fff-far(i-1)).gt.0) cycle
								tar0=tar(i-1)+(fff-far(i-1))*(tar(i)-tar(i-1))/(far(i)-far(i-1))
								if (ttt.gt.tar0) nn=nn+1
								ntot=ntot+1
							end do
							nnn=ntot-nn
							if (abs(mod(nnn,2)).eq.0) www=0.
						end if
					end if
					dvproc=100*dv/v0
					dvan(ifi,itet)=dvan(ifi,itet)+dvproc*www
					vvv(ifi,itet)=vvv(ifi,itet)+www
					!if(itet.eq.101) write(*,*)dv,www
				end do
			end do
		end do

		do ifi=1,nfmap
			do itet=1,ntmap
				vanm=-999.
				if (vvv(ifi,itet).gt.w_limit) then
					vanm=dvan(ifi,itet)/vvv(ifi,itet)
				end if
				v1tmp(ifi,itet)=vanm
			end do
		end do


		aver=0
		naver=0
               dvapr=0
		do ifi=1,nfmap
			do itet=1,ntmap
				vanom=-999
				if(vvv(ifi,itet).gt.w_limit) then
					vanom=v1tmp(ifi,itet)
					aver=aver+vanom
					naver=naver+1
				end if
				v2tmp(ifi,itet)=vanom
                              !dv_aprio=0.0
				if(kod_apriori.eq.1) then
					ttt=(itet-1)*dtmap+tmap1+tet0
					fff=(ifi-1)*dfmap+fmap1+fi0
                                     if (model_type.eq.5) then
                                        call ini_3D_model(fff,ttt,zzz,ips, dva)
                                        dv_aprio=dva
                                        !write(*,*)'dv_aprio=',dva
                                     else
					   call SFDEC(fff,ttt,0.,xxx,yyy,Z,fi0,tet0)
					   dv_aprio = vert_anom(xxx,yyy,zzz,ips)
                                     end if
					v1tmp(ifi,itet)=v1tmp(ifi,itet)+dv_aprio
                                     dvapr(ifi,itet)=dv_aprio
                                    
				end if
				!if(itet.eq.101) write(*,*)vanom,vvv(ifi,itet)
			end do
		end do
		!pause
		if(naver.gt.0) then
			avertot=avertot+aver
			navertot=navertot+naver
			aver=aver/naver
			avlev(ilev)=aver
			write(*,*)'aver= ',aver
		end if
		if(kod_av_bias.eq.1) v1tmp=v1tmp-aver

		do ifi=1,nfmap
			do itet=1,ntmap
				if(vvv(ifi,itet).lt.w_limit) cycle
				vanm=0.
				iv=0
				do iff=-ismth,ismth
					if (ifi+iff.lt.1) cycle
					if (ifi+iff.gt.nfmap) cycle
					do itt=-ismth,ismth
						if (itet+itt.lt.1) cycle
						if (itet+itt.gt.ntmap) cycle
						if(vvv(ifi+iff,itet+itt).lt.w_limit) cycle
						rr=iff*iff+itt*itt
						r=sqrt(rr)
						if(r.gt.rsmth) cycle
						iv=iv+1
						vanm=vanm+v1tmp(ifi+iff,itet+itt)
					end do
				end do
				v2tmp(ifi,itet)=vanm/iv
			end do
		end do

		open(14,file='../../FIG_FILES/HOR/dv'//ps//'_'//lv//'.grd')
		write(14,'(a4)')dsaa
		write(14,*)nfmap,ntmap
		write(14,*)fmap1+fi0,fmap2+fi0
		write(14,*)tmap1+tet0,tmap2+tet0
		write(14,*)-999,999
		do itet=1,ntmap
			write(14,*)(v2tmp(ifi,itet)+add_perc,ifi=1,nfmap)
		end do
		close(14)
		
               open(14,file='../../FIG_FILES/APR/dvapr'//ps//'_'//lv//'.grd')
		write(14,'(a4)')dsaa
		write(14,*)nfmap,ntmap
		write(14,*)fmap1+fi0,fmap2+fi0
		write(14,*)tmap1+tet0,tmap2+tet0
		write(14,*)-999,999
		do itet=1,ntmap
			write(14,*)(dvapr(ifi,itet),ifi=1,nfmap)
		end do
		close(14)

	end do
	avertot=avertot/navertot
	write(*,*)'aver total=', avertot

	if (ips.eq.1) avlevP=avlev
	if (ips.eq.2) avlevS=avlev

end do

if (kod_av_bias.eq.1) then
	open(11,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/ref_av_bias.dat')
	zmin=0
	zmax=32.0

	write(11,*) 0
	do i=1,nrefmod
		zzz=hmod(i)
		do ilev=1,nlev-1
			if (zzz.lt.hlev(1)) then
				z1=zmin
				z2=hlev(1)
				dvp1=avlevP(1)
				dvp2=avlevP(1)
				dvs1=avlevS(1)
				dvs2=avlevS(1)
			else if (zzz.gt.hlev(nlev)) then
				z1=hlev(nlev)
				z2=zmax
				dvp1=avlevP(nlev)
				dvp2=0.
				dvs1=avlevS(nlev)
				dvs2=0.
			else
				z1=hlev(ilev)
				z2=hlev(ilev+1)
				dvp1=avlevP(ilev)
				dvp2=avlevP(ilev+1)
				dvs1=avlevS(ilev)
				dvs2=avlevS(ilev+1)
			end if

			if ((zzz-z1)*(zzz-z2).gt.0) cycle
			exit
		end do

		dvp=dvp1+(zzz-z1)*(dvp2-dvp1)/(z2-z1)
		dvs=dvs1+(zzz-z1)*(dvs2-dvs1)/(z2-z1)
		if (zzz.gt.zmax) then
			dvp=0
			dvs=0
		end if
		write(*,*)'z= ',zzz,dvp,dvs
		vmodp(i)=vmodp(i)*(1+0.01*dvp)
		vmods(i)=vmods(i)*(1+0.01*dvs)
			
		write(11,*) hmod(i),vmodp(i),vmods(i)
	end do
	close(11)

	open(14,file='../../FIG_FILES/1DMOD/'//re//ar//'_avbias.bln')
	write(14,*)nrefmod
	do i=1,nrefmod
		write(14,*)vmodp(i),-hmod(i)
	end do
	write(14,*)nrefmod
	do i=1,nrefmod
		write(14,*)vmods(i),-hmod(i)
	end do
	close(14)
end if

close(66)
stop
end
