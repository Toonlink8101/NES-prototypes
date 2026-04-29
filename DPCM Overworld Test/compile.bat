@echo.
@echo Compiling...
C:\cc65\bin\ca65 cart.asm -g -o example.o --cpu 6502X
@IF ERRORLEVEL 1 GOTO failure
@echo.
@echo Linking...
C:\cc65\bin\ld65 -o DMC4AV.nes -C example.cfg example.o
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