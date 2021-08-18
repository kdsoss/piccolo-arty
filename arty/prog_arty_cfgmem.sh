#!/bin/sh -xe

#PROJ=piccolo_rtl
#PROJ=piccolo_ip
PROJ=piccolo_mem

BITFILE=$PROJ/$PROJ.runs/impl_1/${PROJ}_wrapper.bit

export BITFILE

echo $BITFILE

vivado -nojournal -notrace -nolog -source ./prog_arty_cfgmem.tcl -mode batch

rm *.jou *.log
