include ../../Makerules
OBJ=mod_3d.o 
OBJ+=../../SUBR/VISUAL/dv_1_gr_xyz_v.o ../../SUBR/INVERS/kram3.o ../../SUBR/VISUAL/prepare_model_v.o ../../SUBR/3D_MODEL/vrefmod.o ../../SUBR/OTHERS/pause.o
BIN=mod_3d.exe

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

