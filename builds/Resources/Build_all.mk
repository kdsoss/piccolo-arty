###  -*-Makefile-*-

# Copyright (c) 2018-2019 Bluespec, Inc. All Rights Reserved

# Build all the "standard" builds and test them
# This Makefile should be invoked in the 'builds' directory

CPU=Piccolo
REPO ?= ..

.PHONY: help
help:
	@echo "    Usage:    make build_all"
	@echo "    This Makefile should be invoked in the 'builds' directory"
	@echo ""
	@echo "    Builds and tests all 'standard' builds, i.e., all combinations of:"
	@echo "            RV32/ RV64"
	@echo "         X  ACIMU/ ACDFIMSU"
	@echo "         X  Bluesim/ iverilog/ verilator"
	@echo ""
	@echo "    (needs Bluespec bsc compiler/Bluesim simulator license)"
	@echo ""
	@echo "    Temporary note: iverilog tests are not automated, pending"
	@echo "        fixing the C-import functionality."

.PHONY: build_all_bluesim
build_all_bluesim:
	make  -f $(REPO)/builds/Resources/Build_all.mk  ARCH=RV32AIMU     SIM=bluesim    RVFI_DII=RVFI_DII  build_and_test
	make  -f $(REPO)/builds/Resources/Build_all.mk  ARCH=RV32ADFIMSU  SIM=bluesim    RVFI_DII=RVFI_DII  build_and_test
	make  -f $(REPO)/builds/Resources/Build_all.mk  ARCH=RV64AIMU     SIM=bluesim    RVFI_DII=RVFI_DII  build_and_test
	make  -f $(REPO)/builds/Resources/Build_all.mk  ARCH=RV64ADFIMSU  SIM=bluesim    RVFI_DII=RVFI_DII  build_and_test

.PHONY: build_all_verilator
build_all_verilator:
	make  -f $(REPO)/builds/Resources/Build_all.mk  ARCH=RV32AIMU     SIM=verilator  RVFI_DII=RVFI_DII  build_and_test
	make  -f $(REPO)/builds/Resources/Build_all.mk  ARCH=RV32ADFIMSU  SIM=verilator  RVFI_DII=RVFI_DII  build_and_test
	make  -f $(REPO)/builds/Resources/Build_all.mk  ARCH=RV64AIMU     SIM=verilator  RVFI_DII=RVFI_DII  build_and_test
	make  -f $(REPO)/builds/Resources/Build_all.mk  ARCH=RV64ADFIMSU  SIM=verilator  RVFI_DII=RVFI_DII  build_and_test

.PHONY: build_all_iverilog
build_all_iverilog:
	make  -f $(REPO)/builds/Resources/Build_all.mk  ARCH=RV32AIMU     SIM=iverilog   RVFI_DII=RVFI_DII  build_and_test_iverilog
	make  -f $(REPO)/builds/Resources/Build_all.mk  ARCH=RV32ADFIMSU  SIM=iverilog   RVFI_DII=RVFI_DII  build_and_test_iverilog
	make  -f $(REPO)/builds/Resources/Build_all.mk  ARCH=RV64AIMU     SIM=iverilog   RVFI_DII=RVFI_DII  build_and_test_iverilog
	make  -f $(REPO)/builds/Resources/Build_all.mk  ARCH=RV64ADFIMSU  SIM=iverilog   RVFI_DII=RVFI_DII  build_and_test_iverilog

.PHONY: build
build:
	$(REPO)/builds/Resources/mkBuild_Dir.py  $(REPO)  $(ARCH)  $(SIM) $(RVFI_DII)
	logsave  build_and_test.log  make -C  $(ARCH)_$(CPU)_$(SIM) compile simulator
	mv  build_and_test.log  $(ARCH)_$(CPU)_$(SIM)$

.PHONY: test
test:
	logsave  build_and_test.log  make -C  $(ARCH)_$(CPU)_$(SIM) isa_tests
	mv  build_and_test.log  $(ARCH)_$(CPU)_$(SIM)$

.PHONY: build_and_test
build_and_test: build test

build_all:  build_all_bluesim  build_all_verilator  build_all_iverilog

.PHONY: build_and_test_iverilog
build_and_test_iverilog:
	$(REPO)/builds/Resources/mkBuild_Dir.py  $(REPO)  $(ARCH)  $(SIM) $(RVFI_DII)
	logsave  build_and_test.log  make -C  $(ARCH)_$(CPU)_$(SIM)  all
	mv  build_and_test.log  $(ARCH)_$(CPU)_$(SIM)

.phony: full_clean
full_clean:
	make  -C RV32ACIMU_$(CPU)_bluesim     full_clean
	make  -C RV32ACDFIMSU_$(CPU)_bluesim  full_clean
	make  -C RV64ACIMU_$(CPU)_bluesim     full_clean
	make  -C RV64ACDFIMSU_$(CPU)_bluesim  full_clean
#
	make  -C RV32ACIMU_$(CPU)_verilator     full_clean
	make  -C RV32ACDFIMSU_$(CPU)_verilator  full_clean
	make  -C RV64ACIMU_$(CPU)_verilator     full_clean
	make  -C RV64ACDFIMSU_$(CPU)_verilator  full_clean
#
	make  -C RV32ACIMU_$(CPU)_iverilog     full_clean
	make  -C RV32ACDFIMSU_$(CPU)_iverilog  full_clean
	make  -C RV64ACIMU_$(CPU)_iverilog     full_clean
	make  -C RV64ACDFIMSU_$(CPU)_iverilog  full_clean
