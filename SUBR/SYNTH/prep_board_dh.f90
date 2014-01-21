subroutine prep_board_dh(ar)
character*8 ar
common/brd_dh/fcr1,fcr2,dfcr1,dfcr2,&
			  tcr1,tcr2,dtcr1,dtcr2,ampcr

open(1,file='../../DATA/'//ar//'/INI_PARAM/an_moho.dat')
read(1,*)				
read(1,*)				
read(1,*)ampcr			
read(1,*)fcr1,fcr2,dfcr1,dfcr2				
read(1,*)tcr1,tcr2,dtcr1,dtcr2				
write(*,*)ampcr			
write(*,*)fcr1,fcr2,dfcr1,dfcr2				
write(*,*)tcr1,tcr2,dtcr1,dtcr2				
close(1)


return
end