include ../../Makerules
OBJ=dispers.o goal_new.o halftrace.o prepare_ref.o ray_xyz.o read_z_limit.o refmod_all.o reftrace.o vrefmod.o z_lim.o  

%.o:%.f90
	$(FC) -c $(FFLAGS) $<

all: compile

compile: $(OBJ)

clean:
	@rm -f $(OBJ)
