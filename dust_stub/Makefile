FC = gfortran

PLATFORM:=$(strip $(shell uname)) 
#Add -static-libgcc -static-libgfortran to build static binaries
FCFLAGS_ALL:= -fbackslash
PROGRAM=COYUs9
OLDPROGRAM=COYU9

ifeq ($(PLATFORM), Linux )
FCFLAGS= $(FCFLAGS_ALL)
PLATFORM_KNOWN:=1
endif

ifeq ($(PLATFORM), MINGW32_NT-6.2 )
#We don't build static anymore as the windows installer ships the necessary DLL files from MinGW
#FCFLAGS= $(FCFLAGS_ALL) -static-libgcc -static-libgfortran
FCFLAGS= $(FCFLAGS_ALL)
PROGRAM=COYUs9.exe
OLDPROGRAM=COYU9.exe
PLATFORM_KNOWN:=1
endif

ifndef PLATFORM_KNOWN
$(error Platform $(PLATFORM) is unknown)
endif

.PHONY: all clean distclean showwindeps

all: $(PROGRAM) $(OLDPROGRAM)
	@echo "Built on $(PLATFORM)"

clean:
	rm -f *.o *.mod *.MOD $(PROGRAM) 
	rm -f $(OLDPROGRAM)	

distclean: clean

showwindeps: all
	objdump -x $(PROGRAM) | grep "DLL"

%.exe: %.o
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

%: %.o
	$(FC) $(FCFLAGS) -o $@ $^ $(LDFLAGS)

%.o: %.for 
	$(FC) $(FCFLAGS) -c $<

%.o: %.f90
	$(FC) $(FCFLAGS) -c $<

%.o: %.F90
	$(FC) $(FCFLAGS) -c $<
