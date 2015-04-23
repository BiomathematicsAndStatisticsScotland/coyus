FC = gfortran

PLATFORM:=$(strip $(shell uname)) 

ifeq ($(PLATFORM), Linux )
FCFLAGS= -g
endif

ifeq ($(PLATFORM), MINGW32_NT-6.1 )
FCFLAGS= -g -static-libgcc -static-libgfortran
endif

ifeq ($(PLATFORM), MINGW32_NT-6.1 )
PROGRAM=COYUs9.exe
OLDPROGRAM=COYU9.exe
else 
PROGRAM=COYUs9
OLDPROGRAM=COYU9
endif

.PHONY: all clean distclean showwindeps

all: $(PROGRAM) $(OLDPROGRAM)

clean:
	rm -f *.o *.mod *.MOD $(PROGRAM) 
	rm -f $(OLDPROGRAM)	

distclean: clean

showwindeps: all
	objdump -x $(PROGRAM) | grep "DLL"

%: %.o
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

%.o: %.for 
	$(FC) $(FCFLAGS) -c $<

%.o: %.f90
	$(FC) $(FCFLAGS) -c $<

%.o: %.F90
	$(FC) $(FCFLAGS) -c $<
