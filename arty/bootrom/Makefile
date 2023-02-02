
CROSS_COMPILE?=riscv64-unknown-elf-
GCC=$(CROSS_COMPILE)gcc
OBJCOPY=$(CROSS_COMPILE)objcopy

all: bootrom.elf bootrom.coe

%.coe: %.bin
	hexdump -e '16/4 "%02x " "\n"' $*.bin > $*.hex
	python3 genCoe.py > $@

%.bin: %.elf
	$(OBJCOPY) -O binary $< $@

%.elf: %.S linker.ld
	$(GCC) -g -march=rv32ima -mabi=ilp32 -Tlinker.ld $< -nostdlib -static -Wl,--no-gc-sections -o $@

clean:
	rm -f *.hex *.coe *.bin *.elf
