all:
	make -C SUBR
	make -C LIN_PROG
	make -C 1D_MODEL
	make -C PROGRAM
	make -C VISUAL

makemake:
	(cd SUBR; make clean; ./makemake)
	(cd LIN_PROG; make clean; ./makemake)
	(cd 1D_MODEL; make clean; ./makemake)
	(cd PROGRAM; make clean; ./makemake)
	(cd VISUAL; make clean; ./makemake)

clean:
	make -C SUBR clean
	make -C 1D_MODEL  clean
	make -C LIN_PROG clean
	make -C PROGRAM clean
	make -C VISUAL clean
	

