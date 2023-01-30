@echo off
setlocal enabledelayedexpansion

goto start

:usage
echo Options can include the options described below, as well as the
echo standard Emacs options '--directory', '--funcall', '--load', '--eval',
echo and '--execute'. These Emacs options should be used to ensure that any
echo Elisp files required for the analysis can be found in Emacs' load
echo path.  For package analysis, '-L .' is commonly used. See 'emacs
echo --help' for more information on Emacs options.
echo:
echo Elsa options:
echo:
exit 0

:start
IF "%INSIDE_EMACS%"=="" (
SET EMACS_BIN=emacs
) ELSE (
SET EMACS_BIN="%EMACS%"
)


set EMACS_ARGS=
set ELSA_ARGS=

set addNextArg=false

for %%x in (%*) do (
if "%%x"=="-h"     ( goto usage )
if "%%x"=="--help" ( goto usage )

set add=false

if "!addNextArg!"=="true" (
set add=true
set addNextArg=false
)

set IS_EMACS_ARGS=false

if "%%x"=="-L"          set IS_EMACS_ARGS=true
if "%%x"=="--directory" set IS_EMACS_ARGS=true
if "%%x"=="-f"          set IS_EMACS_ARGS=true
if "%%x"=="--funcall"   set IS_EMACS_ARGS=true
if "%%x"=="-l"          set IS_EMACS_ARGS=true
if "%%x"=="--load"      set IS_EMACS_ARGS=true
if "%%x"=="--eval"      set IS_EMACS_ARGS=true
if "%%x"=="--execute"   set IS_EMACS_ARGS=true

if "!IS_EMACS_ARGS!"=="true" (
set add=true
set addNextArg=true
)

if "!add!"=="true" (
set EMACS_ARGS=!EMACS_ARGS! %%x
) else (
set ELSA_ARGS=!ELSA_ARGS! %%x
)
)

:: This starts Emacs in an environment free from any user-specific
:: configuration:
call %EMACS_BIN% --batch --no-init-file --no-site-file --no-splash ^
%EMACS_ARGS% ^
--eval "(setq enable-dir-local-variables nil)" ^
--load=elsa --funcall=elsa-run %ELSA_ARGS%
