subroutine part_bending_z(ips,val_bend, tout)

common/ray_part/ nodes,xray(10000),yray(10000),zray(10000),sray(10000),kod_tmp(10000)

real xtmp(10000),ytmp(10000),ztmp(10000)
common/shift/ dxray(10000),dyray(10000),dzray(10000)

scur=0
do inode=2,nodes
	s1=sray(inode-1) 
	s2=sray(inode) 
	ds=s2-s1
	scur=scur+ds
end do

sss2=scur/2

sss=-sss2
dxray=0
dyray=0
dzray=0
scur=0
if(abs(val_bend).lt.0.000001) goto 432
do inode=2,nodes-1


	x1=xray(inode-1)
	y1=yray(inode-1)
	z1=zray(inode-1) 
	s1=sray(inode-1) 

	x2=xray(inode)
	y2=yray(inode)
	z2=zray(inode)
	s2=sray(inode)

	ds=s2-s1
	scur=scur+ds


	x3=xray(inode+1)
	y3=yray(inode+1)
	z3=zray(inode+1)

	dh=sqrt((x3-x1)*(x3-x1)+(y3-y1)*(y3-y1))
	dist=sqrt((z3-z1)*(z3-z1)+dh*dh)
	cosa=dh/dist
	sina=(z3-z1)/dist
	cosb=(x3-x1)/dh
	sinb=(y3-y1)/dh

	sss=sss+ds

	d_value = val_bend * (1-(sss*sss)/(sss2*sss2))

	!write(*,*)' sss=',sss,' scur=',scur,' d=',d_value

	dxray(inode)= - d_value * sina * cosb
	dyray(inode)= - d_value * sina * sinb
	dzray(inode)= d_value * cosa
	!write(*,*)inode,dxray(inode),dyray(inode),dzray(inode)

end do
432 continue

xtmp = xray + dxray
ytmp = yray + dyray
ztmp = zray + dzray


!do i=1,nodes
!	write(*,*)xray(i),yray(i),zray(i)
!end do

ttt=0
sss=0
do inode=1,nodes-1
	x1=xtmp(inode)
	x2=xtmp(inode+1)
	xm=(x1+x2)/2.

	y1=ytmp(inode)
	y2=ytmp(inode+1)
	ym=(y1+y2)/2.

	
	z1=ztmp(inode) 
	z2=ztmp(inode+1) 
	zm=(z1+z2)/2.

	!s1=sray(inode) 
	!s2=sray(inode+1) 

	ds=sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1)+(z2-z1)*(z2-z1))
	!ds2=s2-s1
	!write(*,*)inode,ds,ds2

	sss=sss+ds
	vvv=velocity (xm,ym,zm,ips)
	ttt=ttt+ds/vvv
	!write(*,*)' t=',ttt,' zm=',zm,' s=',sss,' v=',vvv
end do
!pause

tout=ttt


!write(*,*)nodes,' zmax=',zmax,' ttt=',ttt,' sss=',sss


return
end