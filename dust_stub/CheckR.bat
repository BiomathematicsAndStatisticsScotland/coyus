@ECHO OFF

REM Checks that R is available and is of the correct version
REM Tested on Win7, should work on WinXP, not sure otherwise. 
SET TESTED_R_VERSION=3.0.2

if defined ProgramFiles(x86) (
	SET REGROOT=hklm\SOFTWARE\Wow6432Node
) else (
    SET REGROOT=hklm\SOFTWARE
)


CALL :fetchreg "%REGROOT%\R-core\R" InstallPath 2 2> NUL

IF NOT DEFINED _result GOTO :R_NOT_FOUND
SET REG_R_HOME=%_result%

CALL :fetchreg "%REGROOT%\R-core\R" "Current Version" 3 2> NUL

IF NOT DEFINED _result GOTO :R_NOT_FOUND:
SET REG_R_VERSION=%_result%

if %REG_R_VERSION% NEQ %TESTED_R_VERSION% (
   ECHO Installed R version is %REG_R_VERSION% 
   ECHO This software was tested with %TESTED_R_VERSION%. 
   ECHO Proceed at your own risk!
   ECHO.
)

FOR %%X IN (Rscript.exe) DO (SET _found=%%~$PATH:X)
IF NOT DEFINED _found GOTO :R_NOT_IN_PATH

ECHO "All checks complete"
ECHO.
pause
EXIT /b

:R_NOT_IN_PATH
	ECHO R is installed but is not in the default path.
	ECHO To correct this issue please set your path to include
	ECHO    %REG_R_HOME%bin
	ECHO The DUST manual has further details in the "Setup" section
	ECHO.
	
REM TODO: use the "setx" command to automatically set the path to what we want. 
EXIT /b 255

:R_NOT_FOUND
	ECHO R does not appear to installed on this computer
	ECHO Please see the DUST manual for directions to download a suitable version of R
	
EXIT /b 255

:fetchreg
SETLOCAL
SET _regkey=%1
SET _regentry=%2
SET _tokens=%3

FOR /f "tokens=%_tokens%*" %%a IN ('reg query %_regkey% /v %_regentry%') DO SET "_dword=%%b"

ENDLOCAL & SET _result=%_dword%
