
open_hw_manager
connect_hw_server -allow_non_jtag
open_hw_target

puts "Booting from configuration memory"
current_hw_device [get_hw_devices xc7a100t_0]
boot_hw_device [current_hw_device]

close_hw_target
disconnect_hw_server
close_hw_manager

puts "Done!"
exit 0

