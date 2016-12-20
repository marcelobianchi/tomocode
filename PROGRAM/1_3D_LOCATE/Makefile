include ../../Makerules
OBJ=3d_locate.o 
OBJ+=../../SUBR/1D_TRACING/halftrace.o ../../SUBR/1D_TRACING/prepare_ref.o ../../SUBR/1D_TRACING/ray_lay.o ../../SUBR/1D_TRACING/ref_time.o ../../SUBR/1D_TRACING/refmod_all.o ../../SUBR/SYNTH/read_ini_model.o ../../SUBR/SYNTH/vert_anom.o ../../SUBR/BENDING_TELE/part_bending_hor.o ../../SUBR/BENDING_TELE/part_bending_z.o ../../SUBR/BENDING_TELE/part_ray.o ../../SUBR/BENDING_TELE/remeshing.o ../../SUBR/BENDING_TELE/trace_1D.o ../../SUBR/BENDING_TELE/trace_bending.o ../../SUBR/CONVERS/decsf.o ../../SUBR/RAYS_LOCATE/dispers.o ../../SUBR/CONVERS/epic_dist.o ../../SUBR/RAZNOE/int_best.o ../../SUBR/INVERS/inv_svd_simple.o ../../SUBR/CONVERS/sfdec.o ../../SUBR/CRUST/moho_depth.o ../../SUBR/CRUST/read_crust.o ../../SUBR/3D_MODEL/anom_3d_xyz_lin_v.o ../../SUBR/3D_MODEL/read_3d_mod_v.o ../../SUBR/RAYS_LOCATE/read_z_limit.o ../../SUBR/3D_MODEL/velocity.o ../../SUBR/3D_MODEL/vrefmod.o ../../SUBR/RAYS_LOCATE/z_lim.o ../../SUBR/SYNTH/read_ini_model_3d.o ../../SUBR/BENDING_TELE/streight_line.o ../../SUBR/OTHERS/pause.o
BIN=3d_locate.exe

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

