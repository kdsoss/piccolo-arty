SECTIONS
{
    ROM_BASE = 0x40400000;

    . = ROM_BASE;
    .text : { *(.text.*) }
    .data : { *(.data.*) }
}
