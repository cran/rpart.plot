@rem webpage.figs.bat:

@"C:\PROGRA~1\R\R-4.1.0\bin\x64\R.exe" CMD BATCH --quiet --vanilla webpage.figs.R
@if %errorlevel% equ 0 goto good1:
@echo R returned errorlevel %errorlevel%, see webpage.figs.Rout:
@echo.
@tail webpage.figs.Rout
@echo webpage.figs.R
@exit /B 1
:good1
diff webpage.figs.Rout webpage.figs.Rout.save
@if %errorlevel% equ 0 goto good2:
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\webpage.figs.save.ps
@exit /B 1
:good2
@rem webpage.figs.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\webpage.figs.save.ps
@if %errorlevel% equ 0 goto good3:
@echo === Files are different ===
@exit /B 1
:good3
@rm -f webpage.figs.Rout
@rm -f Rplots.ps
@exit /B 0
