
open_hw_manager
connect_hw_server -allow_non_jtag
open_hw_target

set DEVICE xc7a100t_0

current_hw_device [lindex [get_hw_devices $DEVICE] 0]

#refresh_hw_device -update_hw_probes false [current_hw_device]

set_property PROBES.FILE {} [current_hw_device]
set_property FULL_PROBES.FILE {} [current_hw_device]
set_property PROGRAM.FILE $env(BITFILE) [current_hw_device]

puts "Programming...\n$env(BITFILE)"
program_hw_devices [current_hw_device]
refresh_hw_device [current_hw_device]

close_hw_target
disconnect_hw_server
close_hw_manager

puts "Done!"
exit 0
