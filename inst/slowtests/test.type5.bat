@rem test.type5.bat:

@"C:\PROGRA~1\R\R-4.1.0\bin\x64\R.exe" CMD BATCH --quiet --vanilla test.type5.R
@if %errorlevel% equ 0 goto good1:
@echo R returned errorlevel %errorlevel%, see test.type5.Rout:
@echo.
@tail test.type5.Rout
@echo test.type5.R
@exit /B 1
:good1
diff test.type5.Rout test.type5.Rout.save
@if %errorlevel% equ 0 goto good2:
@echo === Files are different ===
@diffps -s Rplots.ps ..\..\.#\test-reference\test.type5.save.ps
@exit /B 1
:good2
@rem test.type5.save.ps is too big to be included in the release
@rem so it is stored elsewhere
diffps Rplots.ps ..\..\.#\test-reference\test.type5.save.ps
@if %errorlevel% equ 0 goto good3:
@echo === Files are different ===
@exit /B 1
:good3
@rm -f test.type5.Rout
@rm -f Rplots.ps
@exit /B 0
