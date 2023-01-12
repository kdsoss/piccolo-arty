
open_hw_manager
connect_hw_server -allow_non_jtag
open_hw_target

set DEVICE xc7a100t_0
set CFGMEMPART {s25fl128sxxxxxx0-spi-x1_x2_x4}

current_hw_device [lindex [get_hw_devices $DEVICE] 0]

refresh_hw_device -update_hw_probes false [current_hw_device]
create_hw_cfgmem -hw_device [current_hw_device] -mem_dev [lindex [get_cfgmem_parts $CFGMEMPART] 0]

puts "Generating MCS for: $env(BITFILE)"

write_cfgmem  -format mcs -size 16 -interface SPIx4 -loadbit "up 0x00000000 $env(BITFILE) " \
	-loaddata "up 0x00400000 $env(MEMFILE) " -checksum -force -file "tmp.mcs"

puts "Programming MCS..."

set CFGMEM [get_property PROGRAM.HW_CFGMEM [current_hw_device]]
puts "CFGMEM: $CFGMEM"
set CFGMEMBIT [get_property PROGRAM.HW_CFGMEM_BITFILE [current_hw_device]]
puts "CFGMEMBIT: $CFGMEMBIT"

set_property PROGRAM.ADDRESS_RANGE  {use_file} $CFGMEM
set_property PROGRAM.FILES [list "tmp.mcs" ] $CFGMEM
set_property PROGRAM.PRM_FILE {tmp.prm} $CFGMEM
set_property PROGRAM.UNUSED_PIN_TERMINATION {pull-none} $CFGMEM
set_property PROGRAM.BLANK_CHECK  0 $CFGMEM
set_property PROGRAM.ERASE  1 $CFGMEM
set_property PROGRAM.CFG_PROGRAM  1 $CFGMEM
set_property PROGRAM.VERIFY  1 $CFGMEM
set_property PROGRAM.CHECKSUM  0 $CFGMEM
startgroup 
create_hw_bitstream -hw_device [current_hw_device] $CFGMEMBIT
program_hw_devices [current_hw_device]
refresh_hw_device [current_hw_device];
program_hw_cfgmem -hw_cfgmem $CFGMEM
endgroup

#set_property PROBES.FILE {} [current_hw_device]
#set_property FULL_PROBES.FILE {} [current_hw_device]
#set_property PROGRAM.FILE $env(BITFILE) [current_hw_device]

#puts "Programming...\n$env(BITFILE)"
#program_hw_devices [current_hw_device]
#refresh_hw_device [current_hw_device]

puts "Booting from configuration memory"
boot_hw_device [current_hw_device]
refresh_hw_device [current_hw_device]

close_hw_target
disconnect_hw_server
close_hw_manager

puts "Done!"
exit 0

