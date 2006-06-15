@echo off

rem To install chicken on windows you should:
rem  - set environment variable CHICKEN_HOME to a directory where you want
rem    your poultry to live, e.g. c:\chicken
rem  - add said directory to PATH, so that you can call your chicken
rem    Fri Jun 17 07:51:51 2005 from everywhere
rem
rem This script installs chicken in in CHICKEN_HOME directory (it must be set
rem before running this script

if NOT EXIST chicken.exe goto NotCompiled

if NOT DEFINED CHICKEN_HOME goto ChickenHomeNotDefined

if NOT EXIST "%CHICKEN_HOME%" goto CreateDirAndInstall

rem If CHICKEN_HOME exists, make sure that user specified -force
rem flag. If not, do not install (to prevent accidental overwriting
rem of chicken)

SET FORCE_FLAG=%1

IF NOT DEFINED FORCE_FLAG goto NeedToForce

IF NOT "%FORCE_FLAG%"=="-force" goto NeedToForce
goto InstallChicken

:CreateDirAndInstall
md %CHICKEN_HOME%
:InstallChicken
copy *.exe "%CHICKEN_HOME%"
copy *.dll "%CHICKEN_HOME%"
copy *.lib "%CHICKEN_HOME%"
copy *.exports "%CHICKEN_HOME%"
copy chicken.h "%CHICKEN_HOME%"
copy csibatch.bat "%CHICKEN_HOME%"
copy chicken-ffi-macros.scm "%CHICKEN_HOME%"
copy chicken-more-macros.scm "%CHICKEN_HOME%"

goto Exit

:NeedToForce
echo.
echo CHICKEN_HOME directory ("%CHICKEN_HOME%") already exists. If you're
echo sure you want to install over existing chicken, specify -force flag i.e.
echo win-install.bat -force
goto Exit

:ChickenHomeNotDefined
echo.
echo Please first define CHICKEN_HOME envoronment variable e.g.:
echo SET CHICKEN_HOME=c:\chicken
echo This variable tells us where to install chicken.
goto Exit

:NotCompiled
echo.
echo File chicken.exe is missing. Are you sure you built chicken?
echo To build chicken on Windows using Visual Studio (msvc) do:
echo nmake -f makefile.vc
:Exit
