character*4 dsaa/'DSAA'/

character*8 ar,re,line
character*2 ver
character*1 ps ,rm,it
allocatable dvan(:,:),aprio_dv(:,:),vvv(:,:),vtmp(:,:),vabs(:,:)
real hlev(20)
real fia0(100),teta0(100),fib0(100),tetb0(100)
real fmark(20),tmark(20)


common/pi/pi,per
common/center/fi0,tet0
common/refmod/nrefmod,hmod(600),vmodp(600),vmods(600)
common/crust_model/nmod

one=1.e0
pi=asin(one)*2.e0
per=pi/180.e0
rz=6371.

w_limit=0.2


open(1,file='../../VISUAL/area.dat')
read(1,'(a8)')re
read(1,'(a8)')ar
read(1,*) iter  
write(*,*) iter  
read(1,*) ngr1,ngr2  
read(1,*) add_perc  
read(1,*) kod_av_bias
read(1,*) kod_apriori
write(*,*)' grids:', ngr1,ngr2 
close(1)

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

write(*,*)' AREA : ',ar


!******************************************************************
open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/major_param.dat')
do i=1,10000
	read(1,'(a8)',end=558)line
	if(line.eq.'MOHO MOD') goto 559
end do
558 continue
write(*,*)' cannot find MOHO MODEL in major_param.dat!!!'
pause
559 read(1,*)model_crust
close(1)

nmod=model_crust
write(*,*)'model_crust=',nmod

open(2,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/setver.dat')
open(66,file='../../FIG_FILES/VERT/referencever.dat')
read(2,*)nver
do ii=1,nver
	read(2,*) fia0(ii),teta0(ii),fib0(ii),tetb0(ii)
        write(66,*)  fia0(ii),teta0(ii),fib0(ii),tetb0(ii),ii
end do
read(2,*) dist_from_sec_event
read(2,*) dxsec
read(2,*) zmin,zmax,dzsec
read(2,*) dsmark
read(2,*) dismax
read(2,*) ismth
read(2,*) ind_srce
close(2)
close(66)
write(it,'(i1)')iter


rsmth=ismth+0.5

!call read_3D_mod_v(ar,iter)
call read_crust(model_crust)

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



do ips=1,1
	write(ps,'(i1)')ips


	do iver=1,nver
		write(ver,'(i2)')iver
		fia=fia0(iver)
		teta=teta0(iver)
		fib=fib0(iver)
		tetb=tetb0(iver)
		call SFDEC(fia,teta,0.,xa,ya,Z,fi0,tet0)
		call SFDEC(fib,tetb,0.,xb,yb,Z,fi0,tet0)
		!write(*,*)' xa=',xa,' ya=',ya
		!write(*,*)' xb=',xb,' yb=',yb
		dist=sqrt((xb-xa)*(xb-xa)+(yb-ya)*(yb-ya))
		write(*,*)' section:',ver,' dist=',dist
		sinpov=(yb-ya)/dist
		cospov=(xb-xa)/dist
		nxsec=dist/dxsec+1
		dxsec=dist/(nxsec-1)
		nzsec=(zmax-zmin)/dzsec+1
		dzsec=(zmax-zmin)/(nzsec-1)

		allocate (dvan(nxsec,nzsec),aprio_dv(nxsec,nzsec),vvv(nxsec,nzsec))
		allocate (vtmp(nxsec,nzsec),vabs(nxsec,nzsec))
		vvv=0
		dvan=0


		open(11,file='../../FIG_FILES/VERT/mark_'//ver//'.dat')
		imark=0
		do sss=0.,dist,dsmark
			x=xa+cospov*sss
			y=ya+sinpov*sss
			call decsf(x,y,0.,fi0,tet0,FI,TET,h)
			write(11,*)fi,tet,sss
			imark=imark+1
			fmark(imark)=fi
			tmark(imark)=tet
		end do
		imark=imark+1
		write(11,*)fib,tetb,dist
		fmark(imark)=fib
		tmark(imark)=tetb
		close(11)

	! Draw the position of the section on the surface (line)
		open(11,file='../../FIG_FILES/VERT/mark_'//ver//'.bln')
		write(11,*) imark
		do i=1,imark
			write(11,*)fmark(i),tmark(i)
		end do
		close(11)

		if(ips.eq.1) then
		
		
		    ind_moho=1
		    if (ind_moho.ne.0) then
		        open (12,file='../../FIG_FILES/VERT/moho_'//ver//'.bln')
		        write(12,*) nxsec
		        do ix=1,nxsec
				    sss=(ix-1)*dxsec
				    xcur=xa+((xb-xa)/dist)*sss
				    ycur=ya+((yb-ya)/dist)*sss
				    call decsf(xcur,ycur,0.,fi0,tet0,fff,ttt,h)
				    dhmoho=depth_moho(fff,ttt)
				    write(12,*)sss,-dhmoho
				    !write(*,*)' Moho =',dhmoho, ' ifi =',fff, ' itet =',ttt
				end do
			    close(12)
			end if
    				



			if(ind_srce.ne.0) then
				! Read the coordinates of the stations
				open(2,file='../../DATA/'//re//'/'//ar//'/TIMES/stat_xy.dat')
				open(12,file='../../FIG_FILES/VERT/stat_'//ver//'.dat')
				i=0
				nst1=0
				3	i=i+1
					read(2,*,end=4)xst,yst,zst
					xx1=(xst-xa)*cospov+(yst-ya)*sinpov
					yy1=-(xst-xa)*sinpov+(yst-ya)*cospov
					if(abs(yy1).lt.dist_from_sec_event) then
					    zsf=Rz- sqrt(xst**2+yst**2+(Rz-zst)**2)
						nst1=nst1+1
						write(12,*)xx1,5       !-zsf*1000
					end if
					goto 3
				4	close(2)
				close(12)



				open(1,file='../../DATA/'//re//'/'//ar//'/TIMES/rays_p'//it//'.dat',form='binary')
				open(11,file='../../FIG_FILES/VERT/ztr_'//ver//'.dat')
				nzt1=0
				nzt=0
			872	read(1,end=871)xzt,yzt,zzt,nkrat,key_reloc
				nzt=nzt+1
				xx1=(xzt-xa)*cospov+(yzt-ya)*sinpov
				yy1=-(xzt-xa)*sinpov+(yzt-ya)*cospov

				if(abs(yy1).lt.dist_from_sec_event) then
					nzt1=nzt1+1
					write(11,*)xx1,-zzt
				end if
				do ikr=1,nkrat
					read(1,end=871)i1,i2,a1,a2
				end do
				goto 872
			871 close(1)
				close(11)
				write(*,*)' nst1=',nst1,' nzt1=',nzt1
			end if


		end if



		do igr=ngr1,ngr2
			call prepare_model_v(re,ar,ips,iter,igr)

			do ix=1,nxsec
				sss=(ix-1)*dxsec
				!write(*,*)' ix=',ix,' sss=',sss,' dist=',dist
				!sss=190
				!if(mod(ix,10).eq.0)write(*,*)' ix=',ix,' sss=',sss
				xcur=xa+((xb-xa)/dist)*sss
				ycur=ya+((yb-ya)/dist)*sss
				call decsf(xcur,ycur,0.,fi0,tet0,fff,ttt,h)



				!write(*,*)' xcur=',xcur,' ycur=',ycur
				!write(*,*)' fi=',fff,' tet=',ttt
				do iz=1,nzsec
					zcur=zmin+(iz-1)*dzsec
					vref=vrefmod(zcur,ips)
					if(igr.eq.ngr1) then
						aprio_dv(ix,iz)=0
						if(kod_apriori.eq.1) then
                                                     if (model_type.eq.5) then
                                                        call ini_3D_model(fff,ttt,zcur,ips, dva)
                                                        aprio_dv(ix,iz)=dva
                                                     else
					                   aprio_dv(ix,iz) = vert_anom(xcur,ycur,zcur,ips)
                                                     end if
						end if
					end if
					!zcur=15
					!write(*,*)' avmoho=',avmoho,' z_moho=',z_moho

					call dv_1_grid_v(fff,ttt,zcur,dismax,   dv,umn)

!					 if (umn.gt.0.01) then
!                                         write(*,*)' zcur=',zcur,' dv=',dv,' umn=',umn,'dva=',dva
!					    pause
!                                      end if

					dvan(ix,iz) = dvan(ix,iz) + dv*umn
					vvv(ix,iz)=vvv(ix,iz)+umn
				end do
			end do
		end do



!***************************************************************
!***************************************************************
!***************************************************************

		do iz=nzsec,1,-1
			zcur=zmin+iz*dzsec
                      v0=vrefmod(zcur,ips)
			do ix=1,nxsec
				vanom=-999
                              if(vvv(ix,iz).gt.0.0001) then
					vanom=aprio_dv(ix,iz)+100*(dvan(ix,iz)/vvv(ix,iz))/v0
				end if
				dvan(ix,iz)=vanom !+aprio_dv(ix,iz)
                              !write(*,*)vanom, aprio_dv(ix,iz)
			end do
		end do


! smoothing:
		vtmp=dvan
		do iz=1,nzsec
			do ix=1,nxsec
				dvan(ix,iz)=-999
				if(vvv(ix,iz).lt.0.01) cycle
				vanm=0.
				iv=0
				do ixx=-ismth,ismth
					if (ix+ixx.lt.1) cycle
					if (ix+ixx.gt.nxsec) cycle
					do izz=-ismth,ismth
						if (iz+izz.lt.1) cycle
						if (iz+izz.gt.nzsec) cycle
						if(vvv(ix+ixx,iz+izz).lt.0.01) cycle
						rr=ixx*ixx+izz*izz
						r=sqrt(rr)
						if(r.gt.rsmth) cycle
						iv=iv+1
						vanm=vanm+vtmp(ix+ixx,iz+izz)
					end do
				end do
				dvan(ix,iz)=vanm/iv
			end do
		end do


		aver=0
		naver=0
		do iz=1,nzsec
			zcur=zmin+iz*dzsec
			v0=vrefmod(zcur,ips)
			do ix=1,nxsec
				vanom=-999
				vab=-999
				if(vvv(ix,iz).gt.w_limit) then
					!vanom=100*dvan(ix,iz)/v0
					!dvan(ix,iz)=vanom !+aprio_dv(ix,iz)
					vab=v0*(1+0.01*dvan(ix,iz))
					aver=aver+dvan(ix,iz)
					naver=naver+1
				end if
				vabs(ix,iz)=vab
			end do
		end do
		!pause
		aver=aver/naver
		!dvan=dvan-aver

		open(14,file='../../FIG_FILES/VERT/ver_'//ps//ver//'.grd')
		write(14,'(a4)')dsaa
		write(14,*)nxsec,nzsec
		write(14,*)0,dist
		write(14,*)-zmax,-zmin
		write(14,*)-999,999
		do iz=nzsec,1,-1
			write(14,*)(dvan(ix,iz)+add_perc,ix=1,nxsec)
			!write(14,*)(aprio_dv(ix,iz),ix=1,nxsec)
		end do
		close(14)

		open(14,file='../../FIG_FILES/VERT/abs_'//ps//ver//'.grd')
		write(14,'(a4)')dsaa
		write(14,*)nxsec,nzsec
		write(14,*)0,dist
		write(14,*)-zmax,-zmin
		write(14,*)-9999,999
		do iz=nzsec,1,-1
			write(14,*)(vabs(ix,iz),ix=1,nxsec)
		end do
		close(14)

		deallocate(dvan,aprio_dv,vvv,vtmp,vabs)

	end do


end do

!pause
stop
end
