include ../../Makerules
OBJ=inv_svd_simple.o kram3.o nolet.o  

%.o:%.f90
	$(FC) -c $(FFLAGS) $<

all: compile

compile: $(OBJ)

clean:
	@rm -f $(OBJ)
