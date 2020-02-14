PROGRAM = nnTest
VERSION = 1.0
OBJECTS =  mod_common.o mod_settings.o mod_trainingData.o mod_nn.o main.o

INST = $(HOME)/bin
HOST = $(shell hostname)


# hobbit.uni-mb.si (Intel fortran)
#-------
#ifneq (,$(findstring hobbit,$(HOST)))
#    FC = mpif90
    FC = gfortran
    KON =
# Debug version    
#   OPT = -g -fbounds-check
#   OPDE= d

# Optimized version
    OPT = -O3
    OPDE= o  

FCOPTS = -ffixed-line-length-132  -fcray-pointer #-fdefault-real-8
LDOPTO = 

#endif



all: $(OBJECTS)
	$(FC) -o $(PROGRAM) $(FCOPTS) $(OPT) $(OBJECTS) $(LDOPTO)
	
clean:
	 rm -f *.o *.mod .#*
	 
$(OBJECTS): %.o: %.f90
	 $(FC) $(FCOPTS) $(OPT) -c $<
	 
install:
	 cp $(PROGRAM) $(INST)/$(PROGRAM)-$(VERSION)-$(OPDE)$(KON)
	 
#
# Instalacija BLAS in LAPAC knjiznic
# sudo apt-get update
# sudo apt-get install libblas-dev liblapack-dev
#	 
