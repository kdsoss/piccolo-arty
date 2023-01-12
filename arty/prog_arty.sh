#!/bin/sh -xe

DESIGN=piccolo_5

BITFILE=bits/${DESIGN}.bit
#BITFILE=$DESIGN/$DESIGN.runs/impl_1/${DESIGN}_wrapper.bit

export BITFILE

vivado -nojournal -notrace -nolog -source ./prog_arty.tcl -mode batch

rm *.jou *.log
