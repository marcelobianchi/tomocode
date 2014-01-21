include ../../Makerules
OBJ=anom_3d_xyz.o anom_3d_xyz_lin.o anom_3d_xyz_lin_v.o cut_angle.o read_3d_mod.o read_3d_mod_v.o velocity.o velocity_lin.o vrefmod.o vref_smooth.o  

%.o:%.f90
	$(FC) -c $(FFLAGS) $<

all: compile

compile: $(OBJ)

clean:
	@rm -f $(OBJ)
