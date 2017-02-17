@rem test.prp.bat
@rem Stephen Milborrow Nov 2010 Gardens, Cape Town

@echo === test.prp ===============================================
time /T
@"C:\PROGRA~1\R\R-3.3.2\bin\R.exe" CMD BATCH --quiet --vanilla test.prp.R
@if %errorlevel% equ 0 goto good1:
@echo error: R returned errorlevel %errorlevel%, see test.prp.Rout:
@echo.
@tail test.prp.Rout
@echo.
@exit /B 1
:good1
diff -w test.prp.Rout test.prp.Rout.save
@if %errorlevel% equ 0 goto good2:
@echo test.prp.Rout test.prp.Rout.save: files differ
@diffps -s Rplots.ps ../../.#/test.prp.save.ps
@exit /B %errorlevel%
:good2
diffps Rplots.ps ../../.#/test.prp.save.ps
@if %errorlevel% equ 0 goto good3:
@echo Rplots.ps test.prp.save.ps: files differ
@exit /B %errorlevel%
:good3
@rm -f test.prp.Rout
@rm -f Rplots.ps
time /T
@exit /B  0
