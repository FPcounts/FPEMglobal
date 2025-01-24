@ECHO off

SETLOCAL ENABLEEXTENSIONS
SET me=%~n0
SET parent=%~dp0


REM date and time
FOR /F "TOKENS=1 eol=/ DELIMS=/ " %%A IN ('DATE/T') DO SET dd=%%A
FOR /F "TOKENS=1,2 eol=/ DELIMS=/ " %%A IN ('DATE/T') DO SET mm=%%B
FOR /F "TOKENS=1,2,3 eol=/ DELIMS=/ " %%A IN ('DATE/T') DO SET yyyy=%%C

SET "hr=%TIME:~0,2%"
SET "min=%TIME:~3,2%"

SET "todaysdate=%yyyy%-%mm%-%dd%"
SET "nowtime=%hr%%min%"

SET "datetime=%todaysdate%_%nowtime%"


IF EXIST "%~n1_%datetime%.Rout" (
ECHO.
ECHO.File %~n1_%datetime%.Rout already exists. Move or rename it and re-run the batch script.
ECHO.
GOTO STOP
)

ECHO.
ECHO.
ECHO.------------------------------------------------------------
ECHO.
ECHO.STARTING R IN BATCH MODE ON THE FOLLOWING FILE:
ECHO.
ECHO.fullname: %~f1
ECHO.name    : %~n1
ECHO.ext     : %~x1

@ECHO on
START "%~f1" /BELOWNORMAL /MIN R CMD BATCH --no-save --no-restore %1 "%~n1_%datetime%.Rout"
@ECHO off
ECHO.
ECHO.------------------------------------------------------------
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.**********************************************************
ECHO.*          Press any key to close this window.           *  
ECHO.* The R scrip will continue to run in a separate window. *
ECHO.**********************************************************
ECHO.



:STOP
PAUSE
