
open_hw
connect_hw_server
open_hw_target

set DEVICE xc7a100t_0
set CFGMEM {s25fl128sxxxxxx0-spi-x1_x2_x4}

current_hw_device [get_hw_devices $DEVICE]
refresh_hw_device -update_hw_probes false [lindex [get_hw_devices $DEVICE] 0]
create_hw_cfgmem -hw_device [get_hw_devices $DEVICE] -mem_dev [lindex [get_cfgmem_parts $CFGMEM] 0]

#set_property PROBES.FILE {} [get_hw_devices $DEVICE]
#set_property FULL_PROBES.FILE {} [get_hw_devices $DEVICE]
#set_property PROGRAM.FILE $env(BITFILE) [get_hw_devices $DEVICE]

puts "Generating MCS for...\n$env(BITFILE)"

write_cfgmem  -format mcs -size 16 -interface SPIx4 -loadbit "up 0x00000000 $env(BITFILE) " -loaddata "up 0x00400000 $env(MEMFILE) " -checksum -force -file "tmp.mcs"

puts "Programming MCS...\n"

set_property PROGRAM.ADDRESS_RANGE  {use_file} [ get_property PROGRAM.HW_CFGMEM [lindex [get_hw_devices $DEVICE] 0]]
set_property PROGRAM.FILES [list "tmp.mcs" ] [ get_property PROGRAM.HW_CFGMEM [lindex [get_hw_devices $DEVICE] 0]]
set_property PROGRAM.PRM_FILE {tmp.prm} [ get_property PROGRAM.HW_CFGMEM [lindex [get_hw_devices $DEVICE] 0]]
set_property PROGRAM.UNUSED_PIN_TERMINATION {pull-none} [ get_property PROGRAM.HW_CFGMEM [lindex [get_hw_devices $DEVICE] 0]]
set_property PROGRAM.BLANK_CHECK  0 [ get_property PROGRAM.HW_CFGMEM [lindex [get_hw_devices $DEVICE] 0]]
set_property PROGRAM.ERASE  1 [ get_property PROGRAM.HW_CFGMEM [lindex [get_hw_devices $DEVICE] 0]]
set_property PROGRAM.CFG_PROGRAM  1 [ get_property PROGRAM.HW_CFGMEM [lindex [get_hw_devices $DEVICE] 0]]
set_property PROGRAM.VERIFY  1 [ get_property PROGRAM.HW_CFGMEM [lindex [get_hw_devices $DEVICE] 0]]
set_property PROGRAM.CHECKSUM  0 [ get_property PROGRAM.HW_CFGMEM [lindex [get_hw_devices $DEVICE] 0]]
startgroup 
create_hw_bitstream -hw_device [lindex [get_hw_devices $DEVICE] 0] [get_property PROGRAM.HW_CFGMEM_BITFILE [ lindex [get_hw_devices $DEVICE] 0]]
program_hw_devices [lindex [get_hw_devices $DEVICE] 0]
refresh_hw_device [lindex [get_hw_devices $DEVICE] 0];
program_hw_cfgmem -hw_cfgmem [ get_property PROGRAM.HW_CFGMEM [lindex [get_hw_devices xc7a100t_0] 0]]
endgroup

#program_hw_devices [get_hw_devices $DEVICE]
#refresh_hw_device [lindex [get_hw_devices $DEVICE] 0]
close_hw_target
disconnect_hw_server
close_hw

puts "Done!"
exit 0

