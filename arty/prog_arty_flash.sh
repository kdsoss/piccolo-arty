#!/bin/sh -xe

DESIGN=piccolo_3

BITFILE=bits/${DESIGN}.bit
#BITFILE=$DESIGN/$DESIGN.runs/impl_1/${DESIGN}_wrapper.bit
MEMFILE=bits/bootmem.bin
#MEMFILE=bootmem/bootmem.bin

export BITFILE MEMFILE

echo $BITFILE $MEMFILE

vivado -nojournal -notrace -nolog -source ./prog_arty_flash.tcl -mode batch

rm *.jou *.log
