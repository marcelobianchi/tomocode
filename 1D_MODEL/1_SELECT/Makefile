include ../../Makerules
OBJ=select.o 
OBJ+=../../SUBR/CONVERS/decsf.o ../../SUBR/OTHERS/pause.o
BIN=select.exe

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

