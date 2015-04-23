FC = gfortran

WINREPO_DIR:=repo
DOWNLOAD_REPO:=$(shell if [ -d $(WINREPO) ]; then echo "no"; else echo "yes"; fi)
PKGVER:=$(shell cd package && make getversion)

PROGRAM:=$(shell cd package && make getprogram)

ZIP_CONTENTS:=$(PROGRAM) COYUs9.f90 \
	      COYU_base.R IORoutines.R COYUsRunner.R CoyuRunnerFunctions.R setupFunctions.R \
	      Fix-Paths.ps1 CheckR.bat
ZIP_DIR:=windist-$(PKGVER)

# Utility targets
.PHONY: all clean distclean rpackage repodir passwordprotect

all: windist

stub:
	cd dust_stub && $(MAKE) all

rpackage:
	cd package && $(MAKE) dist


repodir: rpackage
	Rscript utils/setupRepo.R $(DOWNLOAD_REPO) $(PKGVER) $(WINREPO_DIR)

clean:
	cd dust_stub && $(MAKE) clean
	cd package && $(MAKE) clean

distclean: clean
	rm -f *.Rout
	rm -f windist.zip
	cd dust_stub && $(MAKE) clean	
	cd package && $(MAKE) distclean

windist: stub repodir
	mkdir -p $(ZIP_DIR)
	cp -R $(WINREPO_DIR) $(ZIP_DIR)
	cd dust_stub && cp $(ZIP_CONTENTS) ../$(ZIP_DIR)
	cd $(ZIP_DIR) && zip -r ../windist-$(PKGVER).zip *
	rm -rf $(ZIP_DIR)

#passwordprotect: 
#	zip -r -e windist.zip $(ZIP_CONTENTS)
