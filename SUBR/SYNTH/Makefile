include ../../Makerules
OBJ=apriori_anom.o dh_anom.o dh_board.o dv_board.o hor_anom.o ini_3d_model.o our_noise.o prepare_noise.o prep_board_dh.o prep_board_dv.o read_hor_an.o read_hor_dh.o read_ini_model_3d.o read_ini_model.o read_vert_an.o read_vert_brd.o velocity.o vert_anom.o vert_brd.o vrefmod.o  

%.o:%.f90
	$(FC) -c $(FFLAGS) $<

all: compile

compile: $(OBJ)

clean:
	@rm -f $(OBJ)
