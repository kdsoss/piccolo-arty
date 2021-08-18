// SPI flash 16M is at 0x40000000 - 0x40ffffff

#define FLASH_START 0x40400000

	.text
	.globl _start
_start:
1:
	//j 1b
	csrr a0, mhartid
	//la a1, _dtb

	li t0, FLASH_START
	jr t0

/*
	.data
	.globl _dtb
	.align 5, 0
_dtb:
	.incbin "devicetree.dtb"
*/
