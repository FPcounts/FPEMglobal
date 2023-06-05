@echo off

ECHO.
ECHO.

ECHO. ================================================================================
ECHO. DATA
ECHO. ================================================================================

rem Rscript -e "setwd('data-raw'); example(source); sourceDir('.')"
rem if %ERRORLEVEL% GEQ 1 PAUSE

ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.


ECHO. ================================================================================
ECHO. DOCUMENT
ECHO. ================================================================================

Rscript -e "devtools::document()"
if %ERRORLEVEL% GEQ 1 PAUSE

ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.


ECHO. ================================================================================
ECHO. INSTALL
ECHO. ================================================================================

Rscript -e "devtools::install(build = TRUE, build_vignettes = TRUE, upgrade = 'never')"
if %ERRORLEVEL% GEQ 1 PAUSE

ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.


ECHO. ================================================================================
ECHO. TESTS
ECHO. ================================================================================

rem Rscript -e "testthat::test_package('FPEMglobal')"
rem if %ERRORLEVEL% GEQ 1 PAUSE

Rscript "inst/slowTests/rate_model_global_1549_1519_test.R"
if %ERRORLEVEL% GEQ 1 PAUSE

ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.
ECHO.


ECHO. ================================================================================
PAUSE
