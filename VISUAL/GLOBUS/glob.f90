program globus

real xx(1000),yy(1000)
common/pi/pi,per

one=1.e0
pi=asin(one)*2.e0
per=pi/180.e0
rz=6371.0


fi0=-67
tet0=-26.5
open(10,file='coastal_line.bln')
open(11,file='coast.bln')

101 read(10,*,end=100) np
!write(11,*)np
np1=0
xx=0
yy=0
do ip=1,np
	read(10,*) fi, tet
	call hipc(fi0,tet0,fi,tet,fi0,tet0,epi,az)
	if (epi.gt.120.) cycle
	np1=np1+1
	dist=epi*per*rz
	xx(np1)=dist*cos(per*az)
	yy(np1)=dist*sin(per*az)
end do
if (np1.gt.0) write(11,*)np1
do ii=1,np1
	write(11,*)xx(ii),yy(ii)
end do

goto 101

100 close(10)
close(11)

open (11,file='circles.bln')
do i=1,4
	epi=i*30.
	dist=epi*per*rz
	write(11,*)361
	do ii=0,360
		az=ii
		x=dist*cos(per*az)
		y=dist*sin(per*az)
		write(11,*)x,y
	end do
end do
close(11)

open(12,file='border.bln')
epi=120.
dist=epi*per*rz
write(12,*)361
do ii=0,360
	az=ii
	x=dist*cos(per*az)
	y=dist*sin(per*az)
	write(12,*)x,y
end do

write(12,*)2
x=dist*cos(per*270.)
y=dist*sin(per*270.)
write(12,*)x,y
x=dist*cos(per*90.)
y=dist*sin(per*90.)
write(12,*)x,y

write(12,*)2
x=dist*cos(per*0.)
y=dist*sin(per*0.)
write(12,*)x,y
x=dist*cos(per*180.)
y=dist*sin(per*180.)
write(12,*)x,y

close(12)
		 
open(13,file='ztr.dat')
open(14,file='ztr_glob.dat')
open(15,file='ztr_selected.dat')
201 read(13,*,end=200)fzt,tzt,zzt
call hipc(fi0,tet0,fzt,tzt,fi0,tet0,epi,az)
!if (epi.gt.160.) cycle
np1=np1+1
dist=epi*per*rz
x=dist*cos(per*az)
y=dist*sin(per*az)
write(14,*)x,y,zzt
if (epi.gt.3) then
	if (epi.lt.20) write(15,*)fzt,tzt,zzt
end if
goto 201

200 close(13)
close(14)



stop
end