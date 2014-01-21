include ../../Makerules
OBJ=alin.o disp.o hipc.o int_best.o  

%.o:%.f90
	$(FC) -c $(FFLAGS) $<

all: compile

compile: $(OBJ)

clean:
	@rm -f $(OBJ)
