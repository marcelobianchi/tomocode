include ../../Makerules
OBJ=Invbig.o 
OBJ+=../../SUBR/3D_MODEL/anom_3d_xyz_lin_v.o ../../SUBR/INVERS/nolet.o ../../SUBR/3D_MODEL/read_3d_mod_v.o ../../SUBR/3D_MODEL/velocity.o ../../SUBR/SYNTH/vert_anom.o ../../SUBR/3D_MODEL/vrefmod.o ../../SUBR/OTHERS/pause.o
BIN=invbig.exe

%.o:%.f90
	$(FC) -c $(FFLAGS) $<

all: compile

compile: $(OBJ)
	$(FC)  $(FFLAGS) -o $(BIN) $(OBJ) $(LIBS) 

clean:
	@rm -f *.o $(BIN)

fullclean: clean
	@rm -f $(BIN)

install:
	@cp $(BIN) $(DESTDIR)

