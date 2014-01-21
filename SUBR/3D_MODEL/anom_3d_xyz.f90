function anom_3D_xyz(xxx,yyy,zzz,ips)

real fx3(3),fy3(3),fz3(3),xabc(3),yabc(3),zabc(3)
integer ixabc(3),iyabc(3),izabc(3)

common/mod_3D/dv_3D(2,70,70,10),xx1,nxx,dxx,yy1,nyy,dyy,nzz,hlev(10)
common/zlimits/zmin,zmax

anom_3D_xyz=0

!write(*,*)' xxx=',xxx,' yyy=',yyy,' zzz=',zzz

xx2=xx1+(nxx-1)*dxx
yy2=yy1+(nyy-1)*dyy


if((xxx-xx1)*(xxx-xx2).ge.0) return
if((yyy-yy1)*(yyy-yy2).ge.0) return
if((zzz-zmin)*(zzz-zmax).ge.0) return

do ix=1,nxx-1
	x1=xx1+(ix-1)*dxx
	x2=xx1+ix*dxx
	if((xxx-x1)*(xxx-x2).le.0) goto 11
end do
write(*,*)' problem in anom_3D_xyz:'
write(*,*)' xx1=',xx1,' xx2=',xx2,' xxx=',xxx
pause
11 continue

do iy=1,nyy-1
	y1=yy1+(iy-1)*dyy
	y2=yy1+iy*dyy
	if((yyy-y1)*(yyy-y2).le.0) goto 12
end do
write(*,*)' problem in anom_3D_xyz:'
write(*,*)' yyy=',yyy
pause
12 continue

do iz=1,nzz+1
	if(iz.eq.1) then
		z1=zmin
		z2=hlev(1)
	else if(iz.eq.nzz+1) then
		z1=hlev(nzz)
		z2=zmax
	else
		z1=hlev(iz-1)
		z2=hlev(iz)
	end if
	if((zzz-z1)*(zzz-z2).le.0) goto 13
end do
write(*,*)' problem in anom_3D_xyz:'
write(*,*)' zzz=',zzz
pause
13 continue


if(iz.eq.1) then
	v111=0
	v211=0
	v121=0
	v221=0
else
	v111=dv_3D(ips,ix,iy,iz-1)
	v211=dv_3D(ips,ix+1,iy,iz-1)
	v121=dv_3D(ips,ix,iy+1,iz-1)
	v221=dv_3D(ips,ix+1,iy+1,iz-1)
end if

if(iz.eq.nzz+1) then
	v112=0
	v212=0
	v122=0
	v222=0
else
	v112=dv_3D(ips,ix,iy,iz)
	v212=dv_3D(ips,ix+1,iy,iz)
	v122=dv_3D(ips,ix,iy+1,iz)
	v222=dv_3D(ips,ix+1,iy+1,iz)
end if

v11=v111+((v111-v211)/(x1-x2))*(xxx-x1)
v21=v121+((v121-v221)/(x1-x2))*(xxx-x1)
v12=v112+((v112-v212)/(x1-x2))*(xxx-x1)
v22=v122+((v122-v222)/(x1-x2))*(xxx-x1)

v1=v11+((v11-v21)/(y1-y2))*(yyy-y1)
v2=v12+((v12-v22)/(y1-y2))*(yyy-y1)

anom_3D_xyz=v1+((v1-v2)/(z1-z2))*(zzz-z1)

return
end