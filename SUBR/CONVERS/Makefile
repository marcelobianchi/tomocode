include ../../Makerules
OBJ=decsf.o epic_dist.o GAP_angle.o sfdec.o  

%.o:%.f90
	$(FC) -c $(FFLAGS) $<

all: compile

compile: $(OBJ)

clean:
	@rm -f $(OBJ)
