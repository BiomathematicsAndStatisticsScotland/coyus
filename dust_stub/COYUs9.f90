!
! Run R from the system path on the COYUs R routines.
! This avoids the need to rewrite these routines in FORTRAN.
!
! Later, the approach can be extended to allow other arbitrary
! routines written in R to be executed - this might require modifications to the DUST frontend. 
!
!
! This runner routine kept in FORTRAN for compatibility with other parts of the DUST system.
!
! Note this code requires the GNU extension system() - if portability
! to other compilers is desired the use EXECUTE_COMMAND_LINE (Fortran
! 2008 or simply convert this program to C - latter is probably
! easier.
!
! David Nutter 20/03/2014
!
program COYUsRunner
  implicit none

  character(len=7)::R_EXECUTABLE="Rscript"
  character(len=13)::R_FILE="COYUsRunner.R"
  character(len=20)::COYU_INPUT_FILE="COYUs9.DAT"
  character(len=12)::ERROR_FILE="VBERRORS.DAT"

  !Required to ensure methods is loaded on startup otherwise lme4 package won't work.
  character(len=67)::R_PACKAGES=" --default-packages=datasets,utils,grDevices,graphics,stats,methods"
  character(LEN=180) cmdline 
  integer status
  
  cmdline= R_EXECUTABLE // R_PACKAGES // " --vanilla " // R_FILE // " " // COYU_INPUT_FILE // " " // ERROR_FILE

  !write(*,*) cmdline

  status = system(cmdline)

  if (status .ne. 0) then 
    write(*,99) status,cmdline
99 format("COYUsRunner: system call failed with '",I4,"'. Cmdline is '",A80,"'")
 end if

 call exit(status)
end program
