include ../../Makerules
OBJ=halftrace.o prepare_ref.o ray_lay.o ray_xyz.o refmod_all.o ref_time.o reftrace.o vrefmod.o  

%.o:%.f90
	$(FC) -c $(FFLAGS) $<

all: compile

compile: $(OBJ)

clean:
	@rm -f $(OBJ)
