include ../../Makerules
OBJ=add_matr.o 
OBJ+=../../SUBR/RAZNOE/alin.o ../../SUBR/RAZNOE/int_best.o
BIN=add_matr.exe

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

