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



module constants
  implicit none

    !Ideally we'd pull these from limits.h or w32api but we'll just set them 
  !to the Windows limits for now
  integer, parameter :: PATH_MAX=255 
  integer, parameter :: CMD_LENGTH=2040
end module

module functions
  use constants
  implicit none

contains
  
  character(len=1) function get_path_separator() 
    character(len=path_max) :: path
    call get_environment_variable('PATH',path)
    if (path(1:1) .eq. "/") then
       get_path_separator="/" !Unix
    else
       get_path_separator="\\" !Otherwise assume windows
    end if
    
    get_path_separator=path(1:1)
  end function get_path_separator
end module



!Routine to extract the dirname of the command argument 
subroutine get_file_directory(path, resolved_path)
  use, intrinsic :: iso_c_binding
  use constants

  implicit none

    ! Fortran interface to C functions, realpath() and dirname()
  interface
     function realpath(path,resolved_path) bind(c,name="realpath")
       use, intrinsic :: iso_c_binding
       type(c_ptr) :: realpath
       character(len=1,kind=c_char), intent(in) :: path(*)
       character(len=1,kind=c_char), intent(out) :: resolved_path(*)
     end function realpath

     function dirname(path) bind(c, name="dirname")
       use, intrinsic :: iso_c_binding
       type(c_ptr) :: dirname
       character(len=1,kind=c_char), intent(in) :: path(*)
     end function dirname
  end interface  

  character(len=PATH_MAX), intent(in) :: path
  character(len=PATH_MAX), intent(out) :: resolved_path

  ! Define variables for realpath call
  integer :: n
  character(len=1) :: a(PATH_MAX)
  character(len=1) :: b(PATH_MAX)
  character(len=PATH_MAX) :: aa
  type(c_ptr) :: ptr

 

  ptr=realpath(path, a)
  ptr=dirname(a)

  !Determine the first null char, copying the full path across as we go
  aa=transfer(a,aa)
  resolved_path(:)=""
  do n=1,PATH_MAX
     if(iachar(aa(n:n)).eq.0) exit
     resolved_path(n:n)=aa(n:n)
  end do
end subroutine get_file_directory

program COYUsRunner
  use constants
  use functions
  implicit none

  !Constants for invoking fortran code.
  character(len=*),parameter::R_EXECUTABLE="Rscript"
  character(len=*),parameter::R_FILE="COYUsRunner.R"
  character(len=*),parameter::COYU_INPUT_FILE="COYUs9.DAT"
  character(len=*),parameter::ERROR_FILE="VBERRORS.DAT"

  !Required to ensure methods is loaded on startup otherwise lme4 package won't work.
  character(len=*),parameter::R_PACKAGES=" --default-packages=datasets,utils,grDevices,graphics,stats,methods"
  character(len=1) path_separator


  character(len=CMD_LENGTH) cmdline !Could also be allocatable
  integer status

  character(len=PATH_MAX)::exe_location,exe_dir
  character(len=PATH_MAX),parameter::test_loc = "/home/scratch/coyu/coyus/dust_stub"

  path_separator=get_path_separator()

  call get_command_argument(0, exe_location)  
  call get_file_directory(exe_location, exe_dir)
  
  cmdline= R_EXECUTABLE // R_PACKAGES // " --vanilla "  // TRIM(exe_dir) // path_separator // R_FILE &
// " " // COYU_INPUT_FILE // " " // ERROR_FILE

!exe_dir // path_separator //
  write(*,*) TRIM(cmdline)
  !call exit(0)

  !status = system(TRIM(cmdline))
  call execute_command_line(cmdline, exitstat=status)

  if (status .ne. 0) then 
    write(*,99) status,cmdline
99 format("COYUsRunner: system call failed with '",I4,"'. Cmdline is '",A200,"'")
 end if

 call exit(status)
end program



