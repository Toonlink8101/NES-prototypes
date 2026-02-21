@echo.
@echo Compiling...
C:\cc65\bin\ca65 stableframe.asm -g -o example.o --cpu 6502X
@IF ERRORLEVEL 1 GOTO failure
@echo.
@echo Linking...
C:\cc65\bin\ld65 -o stableframe.nes -C stableframe.cfg example.o
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