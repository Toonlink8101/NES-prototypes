@echo.
@echo Compiling...
C:\cc65\bin\ca65 cart.s -g -o example.o
@IF ERRORLEVEL 1 GOTO failure
@echo.
@echo Linking...
C:\cc65\bin\ld65 -o example.nes -C example.cfg example.o
@IF ERRORLEVEL 1 GOTO failure
@echo.
@echo Success!
@pause
@GOTO endbuild
:failure
@echo.
@echo Build error!
@pause
:endbuild