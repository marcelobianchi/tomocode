subroutine read_vert_brd(re,ar)
character*8 ar,re
common/brd_vert/nprof,fia(10),teta(10),fib(10),tetb(10),ybr1(10),ybr2(10),amp(10,2),&
				xbr1(10,2),xbr2(10,2),dxbr1(10,2),dxbr2(10,2),&
				zbr1(10,2),zbr2(10,2),dzbr1(10,2),dzbr2(10,2)
				

open(1,file='../../DATA/'//re//'/'//ar//'/INI_PARAM/anomaly.dat')
read(1,*)				
read(1,*)				
read(1,*)nprof	
do ipr=1,nprof			
	read(1,*)				
	read(1,*)fia(ipr),teta(ipr),fib(ipr),tetb(ipr)				
	read(1,*)ybr1(ipr),ybr2(ipr)				
	read(1,*)amp(ipr,1)				
	read(1,*)xbr1(ipr,1),xbr2(ipr,1),dxbr1(ipr,1),dxbr2(ipr,1)				
	read(1,*)zbr1(ipr,1),zbr2(ipr,1),dzbr1(ipr,1),dzbr2(ipr,1)				
	read(1,*)amp(ipr,2)				
	read(1,*)xbr1(ipr,2),xbr2(ipr,2),dxbr1(ipr,2),dxbr2(ipr,2)				
	read(1,*)zbr1(ipr,2),zbr2(ipr,2),dzbr1(ipr,2),dzbr2(ipr,2)				
end do

close(1)

return
end