include ../../Makerules
OBJ=locate.o 
OBJ+=../../SUBR/CONVERS/decsf.o ../../SUBR/RAYS_LOCATE/dispers.o ../../SUBR/CONVERS/epic_dist.o ../../SUBR/RAYS_LOCATE/goal_new.o ../../SUBR/RAYS_LOCATE/prepare_ref.o ../../SUBR/RAYS_LOCATE/read_z_limit.o ../../SUBR/RAYS_LOCATE/refmod_all.o ../../SUBR/CONVERS/sfdec.o ../../SUBR/RAYS_LOCATE/vrefmod.o ../../SUBR/RAYS_LOCATE/z_lim.o ../../SUBR/OTHERS/pause.o
BIN=locate.exe

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

