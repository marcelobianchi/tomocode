PROGRAM latlon2xyz
  implicit none
  integer :: narg
  real :: latitude, longitude, altitude, exageration
  real :: fi0, tet0
  real :: x, y, z
  character(LEN=2048) :: filename, line

  ! Check Parameter
  narg=iargc()
  if (narg.lt.1) then
     write(0,302) 'You should supply a multcolumn file.'
     stop
  end if
  
  ! Check Config
  call LOADCONFIG(fi0,tet0)
  if (fi0.EQ.999.0.OR.tet0.EQ.999.0) then 
   write(0,*) 'Error loading config.'
   stop
  end if
  
  ! Read file name with data and open:
  call getarg (1, filename)
  open(51,file=filename,status="old", ERR=10)
  
  !Define exageration
  call getarg(2, line)
  read(line, *) exageration

30 read(51,*,END=20) longitude, latitude, altitude
   call SFDEC(longitude, latitude, 0.0, x, y, z, fi0, tet0)
   z = altitude * exageration
   if (x.ge.-250.and.x.le.250.and.y.ge.-250.and.y.le.250) then
   write(*,'(F13.3,1X,F13.3,1X,F13.3,1X,F10.1)') x, y, z
   end if
   goto 30
  
20 close(51)
  goto 100

10 write (0,302) 'Error reading the file: filename.'
  goto 100

100 continue
  STOP
  
301 FORMAT((F15.2,1X,F15.2,1X,F15.2,1X,A))
302 FORMAT(A)
END PROGRAM latlon2xyz
 
SUBROUTINE LOADCONFIG(fi0, tet0)
  implicit none
  character*8 re, ar, line
  real fi0, tet0
  
  !Initialize
  fi0 = 999.0
  tet0 = 999.0

  !Read Area and Region
  open(50, file='../area.dat')
  read(50,'(a8)')re
  read(50,'(a8)')ar
  write(0,*) 'Region: ',TRIM(re),' Area: ',TRIM(ar)
  close(50)

  open(50, file='../../DATA/'//re//'/'//ar//'/INI_PARAM/major_param.dat')

552 read(50,'(a8)',end=553)line
    if(line.eq.'AREA_CEN') goto 554
    goto 552
  
553 continue
  write(0,*)' cannot find AREA CENTER in major_param.dat!!!'
  return

554 read(50,*) fi0, tet0
  write(0,*) 'Lon=',fi0,'Lat=',tet0
  close(50)  

END SUBROUTINE LOADCONFIG

