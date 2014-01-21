include ../../Makerules
OBJ=dv_1_gr_block_ftz.o dv_1_grid.o dv_1_grid_v.o dv_1_gr_xyz.o dv_1_gr_xyz_v.o prepare_model.o prepare_model_v.o prepare_model_x.o  

%.o:%.f90
	$(FC) -c $(FFLAGS) $<

all: compile

compile: $(OBJ)

clean:
	@rm -f $(OBJ)
