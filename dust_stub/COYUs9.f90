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
! In addition the Win32 API is required (for fullpath support)
!
! David Nutter 20/03/2014
!



module constants
  use, intrinsic :: iso_c_binding
  implicit none

  !Ideally we'd pull these from limits.h or w32api but we'll just set them 
  !to the Windows limits for now
  integer(kind=C_SIZE_T), parameter :: PATH_MAX=255 
  integer, parameter :: CMD_LENGTH=2040
  character(len=*),parameter::ERROR_FILE="VBERRORS.DAT"
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
       get_path_separator='\\' !Otherwise assume windows
    end if
    
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
     function fullpath(resolved_path, path, max_path) bind(c,name="_fullpath")
       use, intrinsic :: iso_c_binding
       type(c_ptr) :: fullpath
       character(len=1,kind=c_char), intent(in) :: path(*)
       character(len=1,kind=c_char), intent(out) :: resolved_path(*)
       integer(kind=C_SIZE_T), intent(in) :: max_path
      end function fullpath 

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

 

  !ptr=realpath(path, a)
  ptr=fullpath(a, path, PATH_MAX)
  ptr=dirname(a)

  !Determine the first null char, copying the full path across as we go
  aa=transfer(a,aa)
  resolved_path(:)=""
  do n=1,PATH_MAX
     if(iachar(aa(n:n)).eq.0) exit
     resolved_path(n:n)=aa(n:n)
  end do
end subroutine get_file_directory

subroutine write_error_file(error_msg)
  use constants
  implicit none

  logical :: error_file_exists
  character (len=*),intent(in) :: error_msg

  !Write our errors to ERROR_FILE
  inquire(file=ERROR_FILE, exist=error_file_exists)
  if (error_file_exists) then
    open(unit=1, file=ERROR_FILE, status="old", position="append", action="write")
  else
    open(unit=1, file=ERROR_FILE, status="new", action="write")
  end if
  write(1, *) TRIM(error_msg)
  close(1)

end subroutine

program COYUsRunner
  use constants
  use functions
  implicit none

  !Constants for invoking fortran code.
  character(len=*),parameter::R_EXECUTABLE="Rscript"
  character(len=*),parameter::R_FILE="COYUsRunner.R"
  !character(len=*),parameter::COYU_INPUT_FILE="COYU9.DAT"
  character(len=*),parameter::COYU_INPUT_FILE="COYUs9.DAT"

  !Required to ensure methods is loaded on startup otherwise lme4 package won't work.
  character(len=*),parameter::R_PACKAGES=" --default-packages=datasets,utils,grDevices,graphics,stats,methods"
  character(len=1) path_separator
  integer, parameter::EFFECTIVE_PATH_MAX = PATH_MAX - LEN(R_FILE)

  character(len=CMD_LENGTH) cmdline !Could also be allocatable
  character(len=CMD_LENGTH) error_msg
  integer status
  integer dust_home_is_set
  logical coyu_input_file_exists

  character(len=PATH_MAX)::exe_location,exe_dir

  path_separator=get_path_separator()

  call get_command_argument(0, exe_location)  
  call get_file_directory(exe_location, exe_dir)
 
  if (LEN_TRIM(exe_dir) == 0) then
    call getcwd(exe_dir)
  end if 

  if (LEN_TRIM(exe_dir) >= PATH_MAX) then
    error_msg=""
    write(error_msg, 96) PATH_MAX, TRIM(exe_dir)
    call write_error_file(error_msg)
    call exit(1)
  end if
96 format("COYUs9.exe: path to directory too long (>=",I3," chars. Path: ",A,"'")

  inquire(file=COYU_INPUT_FILE, exist=coyu_input_file_exists)
  if ( .not. coyu_input_file_exists ) then
    error_msg=""
    call getcwd(exe_dir)
    write(error_msg, 97) COYU_INPUT_FILE, TRIM(exe_dir)
    call write_error_file(error_msg)
    call exit(1)
97 format("COYUs9.exe: input file ",A," does not exist. cwd=",A)
  end if

  !Check if our environment looks sensible. A better way would be to actually check the existence of the R install/Rscript.exe
  !It may also be possible to move the environmental setup into this FORTRAN program and dispense with ExecuteDustShell.bat but this may not be
  !desirable as it makes it harder to test alterations to the environnent, use a different R install and so on. 
  call get_environment_variable("DUST_HOME", status=dust_home_is_set)
  if (dust_home_is_set .eq. 1) then
    error_msg="COYUs9.exe: Environment variable DUST_HOME is not set\n\n"//&
              &"DUST must be run via the shortcut in the Windows Start Menu for module COYUS9 to operate."&
              &"Do not run Dus.exe directly." 
    call write_error_file(error_msg)
    call exit(1)
  end if

  cmdline= R_EXECUTABLE // R_PACKAGES // " --vanilla "  // '"' // TRIM(exe_dir) // path_separator &
// R_FILE // '"' // " " // COYU_INPUT_FILE // " " // ERROR_FILE

  error_msg=""
  write(error_msg, 98) TRIM(cmdline)
  call write_error_file(error_msg)
98 format("COYUs9.exe: invoking R with '",A,"'")

  call execute_command_line(TRIM(cmdline), exitstat=status)

  if (status .ne. 0) then
    error_msg="" 
    write(error_msg,99) status,TRIM(cmdline)
    call write_error_file(error_msg)

99 format("COYUs9.exe: system call failed with '",I4,"'. Cmdline is '",A,"'")
  end if

  call exit(status)
end program



