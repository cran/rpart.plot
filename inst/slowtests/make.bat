@rem rpart.plot/inst/slowtests/make.bat

time /T
@call test.rpart.plot.bat
                        @if %errorlevel% NEQ 0 goto error
@call test.rpart.rules.bat
                        @if %errorlevel% NEQ 0 goto error
@call vignette.bat
                        @if %errorlevel% NEQ 0 goto error
@call rpart.report.bat
                        @if %errorlevel% NEQ 0 goto error
@call usersplits.bat
                        @if %errorlevel% NEQ 0 goto error
@call webpage.figs.bat
                        @if %errorlevel% NEQ 0 goto error
@call test.describe.col.bat
                        @if %errorlevel% NEQ 0 goto error
@call test.palette.bat
                        @if %errorlevel% NEQ 0 goto error
@call test.na.bat
                        @if %errorlevel% NEQ 0 goto error
@call test.imports.bat
                        @if %errorlevel% NEQ 0 goto error
@call test.type5.bat
                        @if %errorlevel% NEQ 0 goto error
@goto done
:error
@echo ==== ERROR ====
:done
time /T
@exit /B  0
