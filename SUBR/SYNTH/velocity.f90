function velocity(x,y,z,ips)
common/nanom/n_anomaly
common/center/fi0,tet0

parameter (Rz=6371.)

dist=sqrt(x**2+y**2)
zsf=Rz - sqrt(dist**2+(Rz-z)**2)

v0=vrefmod(zsf,ips)

!call decsf(x,y,0.,fi0,tet0,fi,tet,h)

if(n_anomaly.eq.1)then
	dv = dv_board(x,y,z,ips)
else if(n_anomaly.eq.2)then
	dv = hor_anom(x,y,z,ips)
else if(n_anomaly.eq.3)then
	dv=vert_anom(x,y,z,ips)
else if(n_anomaly.eq.4)then
	dv=vert_brd(x,y,z,ips)
end if
!write(*,*)' v0=',v0,' dv=',dv

velocity = v0*(1 + 0.01 * dv)



!v0=vref_smooth(z,ips)
!dv = anom_3D_xyz(x,y,z,ips)


!velocity = v0 + dv


return
end