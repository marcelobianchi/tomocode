include ../../Makerules
OBJ=invers.o 
OBJ+=../../SUBR/INVERS/nolet.o ../../SUBR/1D_TRACING/vrefmod.o ../../SUBR/OTHERS/pause.o
BIN=invers.exe

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

