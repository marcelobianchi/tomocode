subroutine part_ray(part1,part2)

common/ray/ nodes,xray(10000),yray(10000),zray(10000)
common/ray_part/ ntmp,xtmp(10000),ytmp(10000),ztmp(10000),stmp(10000),kod_tmp(10000)

real sray(10000)

!nodes=nini
!xray=xini
!yray=yini
!zray=zini

sss=0
sray(1)=0
do inode=1,nodes-1
	x1=xray(inode)
	x2=xray(inode+1)

	y1=yray(inode)
	y2=yray(inode+1)

	z1=zray(inode)
	z2=zray(inode+1)

	ds=sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1)+(z2-z1)*(z2-z1))

	sss=sss+ds
	sray(inode+1)=sss
	!write(*,*)' t=',ttt,' s=',sss,' vvv=',vvv
end do
!write(*,*)' sss=',sss,' nodes=',nodes
sA=part1*sss
sB=part2*sss
stotal=sss

n_A=0
dsmin=9999
do inode=1,nodes
	ds=abs(sray(inode)-sA)
	if(ds.gt.dsmin) cycle
	dsmin=ds
	n_A=inode
end do

n_B=0
dsmin=9999
do inode=1,nodes
	ds=abs(sray(inode)-sB)
	if(ds.gt.dsmin) cycle
	dsmin=ds
	n_B=inode
end do

ntmp=0

do inode=n_A,n_B

	x1=xray(inode)
	y1=yray(inode)
	z1=zray(inode)
	s1=sray(inode)

	ntmp=ntmp+1
	xtmp(ntmp)=x1
	ytmp(ntmp)=y1
	ztmp(ntmp)=z1
	stmp(ntmp)=s1
	kod_tmp(ntmp)=inode
end do

!do i=1,ntmp
	!write(*,*)i,stmp(i)/stotal,xtmp(i),ytmp(i),ztmp(i) 
!end do


return
end