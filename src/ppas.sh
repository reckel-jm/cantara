#!/bin/sh
DoExitAsm ()
{ echo "An error occurred while assembling $1"; exit 1; }
DoExitLink ()
{ echo "An error occurred while linking $1"; exit 1; }
echo Linking /home/wuki/Projects/cantara/src/fpccantaraclitest
OFS=$IFS
IFS="
"
/usr/bin/ld -b elf64-x86-64 -m elf_x86_64  --dynamic-linker=/lib64/ld-linux-x86-64.so.2   -s  -L. -o /home/wuki/Projects/cantara/src/fpccantaraclitest -T /home/wuki/Projects/cantara/src/link30559.res -e _start
if [ $? != 0 ]; then DoExitLink /home/wuki/Projects/cantara/src/fpccantaraclitest; fi
IFS=$OFS
