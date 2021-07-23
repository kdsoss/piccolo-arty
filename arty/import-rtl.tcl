#

# this is where to create the project. change to other folder if needed
set PROJ piccolo_rtl

set FPGA [file dirname [file normalize [info script]]]

create_project $PROJ $FPGA/$PROJ -part xc7a100tcsg324-1
set_property board_part digilentinc.com:arty-a7-100:part0:1.0 [current_project]

#set_property  ip_repo_paths "$FPGA/../src_SSITH_P1/xilinx_ip $FPGA/jtag" [current_project]
#update_ip_catalog

add_files -fileset constrs_1 -norecurse $FPGA/Arty-A7-100-Master.xdc
#import_files -fileset constrs_1 $FPGA/Arty-A7-100-Master.xdc

add_files -fileset constrs_1 -norecurse $FPGA/p1_constraints.xdc
#import_files -fileset constrs_1 $FPGA/p1_constraints.xdc

add_files -norecurse [glob -directory "$FPGA/../src_SSITH_P1/xilinx_ip/hdl" *.v]
add_files -norecurse $FPGA/jtag/xilinx_jtag.v

source $FPGA/$PROJ.tcl

make_wrapper -files [get_files $FPGA/$PROJ/$PROJ.srcs/sources_1/bd/$PROJ/$PROJ.bd] -top
add_files -norecurse $FPGA/$PROJ/$PROJ.srcs/sources_1/bd/$PROJ/hdl/${PROJ}_wrapper.v
update_compile_order -fileset sources_1
set_property top ${PROJ}_wrapper [current_fileset]

update_compile_order -fileset sources_1

