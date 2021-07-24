@rem vignette.bat:

@"C:\PROGRA~1\R\R-4.1.0\bin\x64\R.exe" CMD BATCH --quiet --vanilla vignette.R
@if %errorlevel% equ 0 goto good1:
@echo R returned errorlevel %errorlevel%, see vignette.Rout:
@echo.
@tail vignette.Rout
@echo vignette.R
@exit /B 1
:good1
diff vignette.Rout vignette.Rout.save
@if %errorlevel% equ 0 goto good2:
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\vignette.save.ps
@exit /B 1
:good2
@rem vignette.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\vignette.save.ps
@if %errorlevel% equ 0 goto good3:
@echo === Files are different ===
@exit /B 1
:good3
@rm -f vignette.Rout
@rm -f Rplots.ps
@exit /B 0
