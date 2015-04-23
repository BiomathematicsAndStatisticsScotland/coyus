
set COYU9DAT=%1

if "%~1"=="" GOTO NOFILE

echo "Running with %COYU9DAT%"

Rscript  --default-packages=datasets,utils,grDevices,graphics,stats,methods --vanilla COYUsRunner.R "%COYU9DAT%" "VBERRORS.DAT"

EXIT /b

:NOFILE
echo "Give me a COYU9.DAT file as an argument"
EXIT /b 255
