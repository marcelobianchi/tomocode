include ../../Makerules
OBJ=file.o pause.o  
FFLAGS+=-cpp

%.o:%.f90
	$(FC) -c $(FFLAGS) $<

all: compile

compile: $(OBJ)

clean:
	@rm -f $(OBJ)
