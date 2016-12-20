include ../../Makerules
OBJ=visual_synini_3d.o 
OBJ+=../../SUBR/CONVERS/decsf.f90 ../../SUBR/SYNTH/dv_board.f90 ../../SUBR/SYNTH/hor_anom.f90 ../../SUBR/SYNTH/prep_board_dv.f90 ../../SUBR/SYNTH/read_hor_an.f90 ../../SUBR/SYNTH/read_vert_an.f90 ../../SUBR/SYNTH/read_vert_brd.f90 ../../SUBR/CONVERS/sfdec.f90 ../../SUBR/SYNTH/vert_anom.f90 ../../SUBR/SYNTH/vert_brd.f90 ../../SUBR/SYNTH/vrefmod.f90 ../../SUBR/OTHERS/pause.o
BIN=visual_synini_3d.exe

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

