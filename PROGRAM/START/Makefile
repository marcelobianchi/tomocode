include ../../Makerules
OBJ=common.o ../../SUBR/OTHERS/file.o ../../SUBR/OTHERS/pause.o

%.o:%.f90
	$(FC) -c $(FFLAGS) $<

all: compile

compile: $(OBJ) syn real

syn: synth.o
	$(FC) $(FFLAGS) -o start_synth synth.o $(OBJ) $(LIBS)

real: real.o
	$(FC) $(FFLAGS) -o start_real real.o $(OBJ) $(LIBS)

clean:
	@rm -f *.o $(BIN)

fullclean: clean
	@rm -f start_real start_synth

