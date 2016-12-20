include ../../Makerules
OBJ=file.o pause.o

%.o:%.f90
	$(FC) -c -cpp $(FFLAGS) $<

all: compile

compile: $(OBJ)

clean:
	@rm -f $(OBJ)
