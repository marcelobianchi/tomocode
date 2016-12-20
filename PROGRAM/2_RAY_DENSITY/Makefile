include ../../Makerules
OBJ=plotray.o 
OBJ+=../../SUBR/RAZNOE/int_best.o ../../SUBR/OTHERS/pause.o
BIN=plotray.exe

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

