include ../../Makerules
OBJ=glob.o 
OBJ+=../../SUBR/CONVERS/epic_dist.o ../../SUBR/RAZNOE/hipc.o ../../SUBR/CONVERS/sfdec.o
BIN=glob.exe

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

