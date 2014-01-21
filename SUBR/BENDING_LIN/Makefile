include ../../Makerules
OBJ=part_bending_hor.o part_bending_z.o part_ray.o remeshing.o streight_line.o trace_bending.o  

%.o:%.f90
	$(FC) -c $(FFLAGS) $<

all: compile

compile: $(OBJ)

clean:
	@rm -f $(OBJ)
