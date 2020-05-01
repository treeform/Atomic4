@echo off
echo This will install Atomic 4 on flopy at A: 
echo Press Ctrl+Break to exit
pause

echo Compileing programs
nasm -f bin -oatomic4.bin  atomic4.asm
nasm -f bin -oAtoClock.bin AtoClock.asm 
nasm -f bin -oAtomCalc.bin AtomCalc.asm
nasm -f bin -odebug.bin    debug.asm

echo Copying programs

abscopy atomic4.bin  0
abscopy AtoClock.bin 2
abscopy AtomCalc.bin 3
abscopy debug.bin    5


echo look at the readme.txt and  
echo now you can restart off the flopy
