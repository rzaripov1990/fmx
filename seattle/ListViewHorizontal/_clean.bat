@echo off
title delete temp delphi files
color 02
cls
echo deleting....
del *.dcu /s
del *.ppu /s
del *.~* /s
del *.dsk /s
del *.cfg /s
del *.dof /s
del *.bk? /s
del *.mps /s
del *.rst /s
del *.s /s
del *.o /s
del *.a /s
del *.local /s
del *.identcache /s
del *.tvsconfig /s
echo clear...
exit