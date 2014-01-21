include ../../Makerules
OBJ=moho_depth.o read_crust.o  

%.o:%.f90
	$(FC) -c $(FFLAGS) $<

all: compile

compile: $(OBJ)

clean:
	@rm -f $(OBJ)
