function dh_board(fi,tet)
common/brd_dh/fcr1,fcr2,dfcr1,dfcr2,&
			  tcr1,tcr2,dtcr1,dtcr2,ampcr

dh_board=0

!fi=99
!tet=49

if((fi-fcr1)*(fi-fcr2).gt.0.) return
if((tet-tcr1)*(tet-tcr2).gt.0.) return


nfi=(fcr2-fcr1)/(dfcr1+dfcr2)
ntet=(tcr2-tcr1)/(dtcr1+dtcr2)


do ifi=1,nfi
	f1=fcr1+(ifi-1)*(dfcr1+dfcr2)
	f2=fcr1+ ifi   *(dfcr1+dfcr2)
	if((fi-f1)*(fi-f2).le.0.) exit
end do

!write(*,*)' f1=',f1,' dfcr1=',dfcr1

if(fi.ge.f1+dfcr1) return

do itet=1,ntet
	t1=tcr1+(itet-1)*(dtcr1+dtcr2)
	t2=tcr1+ itet   *(dtcr1+dtcr2)
	if((tet-t1)*(tet-t2).le.0.) exit
end do
!write(*,*)' t1=',t1,' dtcr1=',dtcr1

if(tet.ge.t1+dtcr1) return


sign=(-1.)**(ifi+itet)

dh_board=sign*ampcr
!write(*,*)' sign=',sign,' ampcr=',ampcr


return
end