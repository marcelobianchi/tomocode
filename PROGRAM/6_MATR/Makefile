include ../../Makerules
OBJ=matr.o 
OBJ+=../../SUBR/3D_MODEL/anom_3d_xyz_lin_v.o ../../SUBR/3D_MODEL/read_3d_mod_v.o ../../SUBR/3D_MODEL/velocity.o ../../SUBR/SYNTH/vert_anom.o ../../SUBR/3D_MODEL/vrefmod.o ../../SUBR/SYNTH/read_ini_model_3d.o ../../SUBR/SYNTH/read_ini_model.o ../../SUBR/CONVERS/sfdec.o ../../SUBR/OTHERS/pause.o
BIN=matr.exe

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

