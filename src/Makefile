PROGRAM = nnTest
VERSION = 1.0
OBJECTS =  mod_common.o mod_settings.o mod_trainingData.o mod_nn.o main.o

INST = $(HOME)/bin

FC = gfortran
OPT = -O3
OPDE= o  

FCOPTS = -ffixed-line-length-132  -fcray-pointer 
LDOPTO = 


all: $(OBJECTS)
	$(FC) -o $(PROGRAM) $(FCOPTS) $(OPT) $(OBJECTS) $(LDOPTO)
	
clean:
	 rm -f *.o *.mod .#*
	 
$(OBJECTS): %.o: %.f90
	 $(FC) $(FCOPTS) $(OPT) -c $<
	 
install:
	 cp $(PROGRAM) $(INST)/$(PROGRAM)-$(VERSION)-$(OPDE)$(KON)
	 
