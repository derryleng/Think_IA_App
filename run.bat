@echo %off
setlocal enabledelayedexpansion

set AppDir=%~dp0
set R=C:\Program Files\R\*\bin\x64\R.exe

"%R%" --no-save --slave -e "shiny::runApp(appDir = commandArgs(T)[1], launch.browser = T)" --args "%AppDir%"

exit 0
