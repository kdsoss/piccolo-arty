#!/bin/bash

gdb=gdb-multiarch
elf=LiteOS.elf

$gdb $elf -ex "target remote localhost:3333" -ex "load"
