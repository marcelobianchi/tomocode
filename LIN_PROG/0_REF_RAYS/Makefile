include ../../Makerules
OBJ=refrays.o 
OBJ+=../../SUBR/1D_TRACING/ray_lay.o ../../SUBR/RAYS_LOCATE/reftrace.o ../../SUBR/RAYS_LOCATE/vrefmod.o ../../SUBR/OTHERS/pause.o
BIN=refrays.exe

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

