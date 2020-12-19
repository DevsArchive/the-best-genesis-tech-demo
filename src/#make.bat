@echo off

set OUTPUT=rom.md

..\bin\asm68k /p /o ae- ..\md\mdmain.asm,%OUTPUT%,,list.lst
echo.
..\bin\rompad %OUTPUT% 255 0
..\bin\fixheadr %OUTPUT%

pause