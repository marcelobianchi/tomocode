include ../../Makerules
OBJ=create.o 
OBJ+=../../SUBR/CONVERS/decsf.o ../../SUBR/SYNTH/dv_board.o ../../SUBR/SYNTH/hor_anom.o ../../SUBR/RAZNOE/int_best.o ../../SUBR/SYNTH/prep_board_dv.o ../../SUBR/SYNTH/read_hor_an.o ../../SUBR/SYNTH/read_vert_an.o ../../SUBR/SYNTH/read_vert_brd.o ../../SUBR/CONVERS/sfdec.o ../../SUBR/SYNTH/vert_anom.o ../../SUBR/SYNTH/vert_brd.o
BIN=create.exe

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

