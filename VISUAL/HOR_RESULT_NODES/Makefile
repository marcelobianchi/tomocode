include ../../Makerules
OBJ=visual.o 
OBJ+=../../SUBR/CONVERS/decsf.o ../../SUBR/VISUAL/dv_1_grid_v.o ../../SUBR/RAZNOE/int_best.o ../../SUBR/INVERS/kram3.o ../../SUBR/VISUAL/prepare_model_v.o ../../SUBR/SYNTH/read_ini_model.o ../../SUBR/CONVERS/sfdec.o ../../SUBR/SYNTH/vert_anom.o ../../SUBR/SYNTH/vrefmod.o ../../SUBR/SYNTH/read_ini_model_3d.o ../../SUBR/SYNTH/ini_3d_model.o ../../SUBR/CRUST/read_crust.o ../../SUBR/CRUST/moho_depth.o ../../SUBR/OTHERS/pause.o
BIN=visual.exe

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

