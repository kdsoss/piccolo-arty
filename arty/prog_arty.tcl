
open_hw
connect_hw_server
open_hw_target

set DEVICE xc7a100t_0
set CFGMEM {s25fl128sxxxxxx0-spi-x1_x2_x4}

current_hw_device [get_hw_devices $DEVICE]
#refresh_hw_device -update_hw_probes false [lindex [get_hw_devices $DEVICE] 0]
#create_hw_cfgmem -hw_device [get_hw_devices $DEVICE] -mem_dev [lindex [get_cfgmem_parts $CFGMEM] 0]

set_property PROBES.FILE {} [get_hw_devices $DEVICE]
set_property FULL_PROBES.FILE {} [get_hw_devices $DEVICE]
set_property PROGRAM.FILE $env(BITFILE) [get_hw_devices $DEVICE]

puts "Programming...\n$env(BITFILE)"

program_hw_devices [get_hw_devices $DEVICE]
#refresh_hw_device [lindex [get_hw_devices $DEVICE] 0]
close_hw_target
disconnect_hw_server
close_hw

puts "Done!"
exit 0
