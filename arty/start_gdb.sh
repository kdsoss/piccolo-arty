#!/bin/bash

gdb=gdb-multiarch
elf=bootmem/liteos.elf

$gdb $elf -ex "target extended-remote localhost:3333" -ex "load"
