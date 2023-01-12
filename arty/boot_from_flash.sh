#!/bin/sh -xe

vivado -nojournal -notrace -nolog -source ./boot_from_flash.tcl -mode batch

rm *.jou *.log
