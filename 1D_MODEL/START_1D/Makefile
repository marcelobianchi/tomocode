include ../../Makerules
OBJ=start_real.o 
OBJ+= ../../SUBR/OTHERS/pause.o
BIN=start_real.exe

%.o:%.f90
	$(FC) -c $(FFLAGS) $<

all: compile

compile: $(OBJ)
	$(FC)  $(FFLAGS) -o $(BIN) $(OBJ) $(LIBS) 

clean:
	@rm -f *.o $(BIN)

fullclean: clean
	@rm $(BIN)

install:
	@cp $(BIN) $(DESTDIR)

