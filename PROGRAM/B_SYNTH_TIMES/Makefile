include ../../Makerules
OBJ=rays.o 
OBJ+=../../SUBR/1D_TRACING/halftrace.o ../../SUBR/1D_TRACING/prepare_ref.o ../../SUBR/1D_TRACING/ray_lay.o ../../SUBR/1D_TRACING/ref_time.o ../../SUBR/BENDING_TELE/part_bending_hor.o ../../SUBR/BENDING_TELE/part_bending_z.o ../../SUBR/BENDING_TELE/part_ray.o ../../SUBR/BENDING_TELE/remeshing.o ../../SUBR/BENDING_TELE/trace_1D.o ../../SUBR/BENDING_TELE/trace_bending.o ../../SUBR/CONVERS/decsf.o ../../SUBR/CONVERS/epic_dist.o ../../SUBR/RAZNOE/int_best.o ../../SUBR/CONVERS/sfdec.o ../../SUBR/CRUST/moho_depth.o ../../SUBR/CRUST/read_crust.o ../../SUBR/SYNTH/dv_board.o ../../SUBR/SYNTH/hor_anom.o ../../SUBR/SYNTH/our_noise.o ../../SUBR/SYNTH/prep_board_dv.o ../../SUBR/SYNTH/prepare_noise.o ../../SUBR/SYNTH/read_hor_an.o ../../SUBR/SYNTH/read_vert_an.o ../../SUBR/SYNTH/read_vert_brd.o ../../SUBR/SYNTH/velocity.o ../../SUBR/SYNTH/vert_anom.o ../../SUBR/SYNTH/vert_brd.o ../../SUBR/SYNTH/vrefmod.o ../../SUBR/BENDING_TELE/streight_line.f90 ../../SUBR/OTHERS/pause.o
BIN=rays.exe

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

