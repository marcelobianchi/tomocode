!
! Series of subroutines for handling dirs/files copy/pathnames
! in a windows/linux safe way.
!
! During compile you should define a -DWINDOWS or -DLINUX (Linux) or
! windows /DWINDOWS /DLINUX on your compiler.
!
! BIANCHI (30-Jun-2011)

! Implemented:
!
! AS FUNCTIONS:
!
! logical FUNCTION isfile(inputname)
! integer FUNCTION runcommand(inputname)
!
! AS SUBROUTINES:
!
! SUBROUTINE createdir(inputname)
! SUBROUTINE copyfile(from, to)
! SUBROUTINE doslash (inputname)
!

logical FUNCTION isfile(inputname)
	implicit none
	character(len=*):: inputname
	character*2048:: lcopy
	
	lcopy = TRIM(inputname)
	call doslash(lcopy)
	
	open(unit=999,file=TRIM(lcopy),status='old',ERR=1000)
	ISFILE = .TRUE.
	CLOSE(999)
	RETURN
	
1000	ISFILE = .FALSE.
	RETURN
END FUNCTION

SUBROUTINE createdir(inputname)
	implicit none
	integer i, system
	character(len=*):: inputname
	character*2048:: lcopy
	
	lcopy = TRIM(inputname)
	call doslash(lcopy)
	
#ifdef WINDOWS
	i = system ('mkdir '//TRIM(lcopy))
#elif LINUX
	i = system ('mkdir -p '//TRIM(lcopy))
#else
	write(*,*) 'Compiled without -DWINDOWS or -DLINUX'
#endif
END SUBROUTINE

SUBROUTINE copyfile(from, to)
	implicit none
	integer i, system
	character(len=*):: from,  to
	character*2048::  lfrom, lto
	
	lfrom = TRIM(from)
	lto = TRIM(to)

	call doslash(lfrom)
	call doslash(lto)
	
#ifdef WINDOWS
	i = system ('copy '//TRIM(lfrom)//' '//TRIM(lto))
#elif LINUX
	i = system ('cp -v '//TRIM(lfrom)//' '//TRIM(lto))
#else
	write(*,*) 'Compiled without -DWINDOWS or -DLINUX'
#endif
END SUBROUTINE

integer FUNCTION runcommand(inputname)
	implicit none
	integer system
	character(len=*):: inputname
	character*2048::  linput
	
	linput = TRIM(inputname)
	call doslash(linput)
	write (*,*) 'Running command: ', linput(1:LEN(TRIM(linput)))
	runcommand = system(linput)
	RETURN
END FUNCTION

SUBROUTINE doslash (inputname)
	implicit none
	character(len=*):: inputname
	integer i, count, n
	character from, to
	
#ifdef WINDOWS
	from = '/'
	to = '\'
#elif LINUX
	from = '\'
	to = '/'
#else
	write(*,*) 'Compiled without -DWINDOWS or -DLINUX'
	from = '/'
	to = '/'
#endif
	
	n = LEN(TRIM(inputname))
	count=0
	DO i=1, n
		if (inputname(i:i).EQ.from) inputname(i:i) = to
	ENDDO
END SUBROUTINE

