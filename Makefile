# ======================================================================
# Makefile : GPECN1DTDPBC
#
# TESTED
#
#     GNU Make 3.81
#     GNU Fortran (Homebrew GCC 6.3.0_1) 6.3.0
#
# LAST TESTED
# 
#     GNU Make 3.81
#     GNU Fortran (Homebrew GCC 6.3.0_1) 6.3.0
#
# LAST UPDATED
#
#     Saturday, March 25th, 2017
#
# ----------------------------------------------------------------------
COMPILER := gfortran
COMPILER_OPTIONS := -fimplicit-none \
                    -ffree-form \
                    -ffree-line-length-none \
                    -std=gnu \
                    -O3 \
                    -mtune=native \
                    -fdefault-real-8 \
                    -fdefault-double-8
SOURCE_DIR := source
LAPACK_DIR := libraries/lapack
BUILD_DIR := build

all: gpecn1Dtdpbc.x \
     wavefunc.x \
     potential.x \
     nonlinear.x \
     expectation.x \
     ftpsd.x \
     binary.x \
     momentum.x \
     spacetime.x \
     freqtime.x \

gpecn1Dtdpbc.x: $(BUILD_DIR)/gpecn1Dtdpbc.o \
                $(BUILD_DIR)/zgtsv.o \
                $(BUILD_DIR)/xerbla.o
	$(COMPILER) $(COMPILER_OPTIONS) \
          $(BUILD_DIR)/gpecn1Dtdpbc.o \
          $(BUILD_DIR)/zgtsv.o \
          $(BUILD_DIR)/xerbla.o \
          -o gpecn1Dtdpbc.x

$(BUILD_DIR)/gpecn1Dtdpbc.o: $(SOURCE_DIR)/gpecn1Dtdpbc.f
	$(COMPILER) $(COMPILER_OPTIONS) \
          -c $(SOURCE_DIR)/gpecn1Dtdpbc.f \
          -o $(BUILD_DIR)/gpecn1Dtdpbc.o

$(BUILD_DIR)/zgtsv.o: $(LAPACK_DIR)/zgtsv.f
	$(COMPILER) $(COMPILER_OPTIONS) \
          -c $(LAPACK_DIR)/zgtsv.f \
          -o $(BUILD_DIR)/zgtsv.o

$(BUILD_DIR)/xerbla.o: $(LAPACK_DIR)/xerbla.f
	$(COMPILER) $(COMPILER_OPTIONS) \
          -c $(LAPACK_DIR)/xerbla.f \
          -o $(BUILD_DIR)/xerbla.o

wavefunc.x: $(BUILD_DIR)/wavefunc.o
	$(COMPILER) $(COMPILER_OPTIONS) \
          $(BUILD_DIR)/wavefunc.o \
          -o wavefunc.x

$(BUILD_DIR)/wavefunc.o: $(SOURCE_DIR)/wavefunc.f
	$(COMPILER) $(COMPILER_OPTIONS) \
          -c $(SOURCE_DIR)/wavefunc.f \
          -o $(BUILD_DIR)/wavefunc.o

potential.x: $(BUILD_DIR)/potential.o
	$(COMPILER) $(COMPILER_OPTIONS) \
          $(BUILD_DIR)/potential.o \
          -o potential.x

$(BUILD_DIR)/potential.o: $(SOURCE_DIR)/potential.f
	$(COMPILER) $(COMPILER_OPTIONS) \
          -c $(SOURCE_DIR)/potential.f \
          -o $(BUILD_DIR)/potential.o

nonlinear.x: $(BUILD_DIR)/nonlinear.o
	$(COMPILER) $(COMPILER_OPTIONS) \
          $(BUILD_DIR)/nonlinear.o \
          -o nonlinear.x

$(BUILD_DIR)/nonlinear.o: $(SOURCE_DIR)/nonlinear.f
	$(COMPILER) $(COMPILER_OPTIONS) \
          -c $(SOURCE_DIR)/nonlinear.f \
          -o $(BUILD_DIR)/nonlinear.o

expectation.x: $(BUILD_DIR)/expectation.o
	$(COMPILER) $(COMPILER_OPTIONS) \
          $(BUILD_DIR)/expectation.o \
          -o expectation.x

$(BUILD_DIR)/expectation.o: $(SOURCE_DIR)/expectation.f
	$(COMPILER) $(COMPILER_OPTIONS) \
          -c $(SOURCE_DIR)/expectation.f \
          -o $(BUILD_DIR)/expectation.o

ftpsd.x: $(BUILD_DIR)/ftpsd.o
	$(COMPILER) $(COMPILER_OPTIONS) \
          $(BUILD_DIR)/ftpsd.o \
          -o ftpsd.x

$(BUILD_DIR)/ftpsd.o: $(SOURCE_DIR)/ftpsd.f
	$(COMPILER) $(COMPILER_OPTIONS) \
          -c $(SOURCE_DIR)/ftpsd.f \
          -o $(BUILD_DIR)/ftpsd.o

binary.x: $(BUILD_DIR)/binary.o
	$(COMPILER) $(COMPILER_OPTIONS) \
          $(BUILD_DIR)/binary.o \
          -o binary.x

$(BUILD_DIR)/binary.o: $(SOURCE_DIR)/binary.f
	$(COMPILER) $(COMPILER_OPTIONS) \
          -c $(SOURCE_DIR)/binary.f \
          -o $(BUILD_DIR)/binary.o

momentum.x: $(BUILD_DIR)/momentum.o
	$(COMPILER) $(COMPILER_OPTIONS) \
          $(BUILD_DIR)/momentum.o \
          -o momentum.x

$(BUILD_DIR)/momentum.o: $(SOURCE_DIR)/momentum.f
	$(COMPILER) $(COMPILER_OPTIONS) \
          -c $(SOURCE_DIR)/momentum.f \
          -o $(BUILD_DIR)/momentum.o

spacetime.x: $(BUILD_DIR)/spacetime.o
	$(COMPILER) $(COMPILER_OPTIONS) \
          $(BUILD_DIR)/spacetime.o \
          -o spacetime.x

$(BUILD_DIR)/spacetime.o: $(SOURCE_DIR)/spacetime.f
	$(COMPILER) $(COMPILER_OPTIONS) \
          -c $(SOURCE_DIR)/spacetime.f \
          -o $(BUILD_DIR)/spacetime.o

freqtime.x: $(BUILD_DIR)/freqtime.o
	$(COMPILER) $(COMPILER_OPTIONS) \
          $(BUILD_DIR)/freqtime.o \
          -o freqtime.x

$(BUILD_DIR)/freqtime.o: $(SOURCE_DIR)/freqtime.f
	$(COMPILER) $(COMPILER_OPTIONS) \
          -c $(SOURCE_DIR)/freqtime.f \
          -o $(BUILD_DIR)/freqtime.o

.PHONY: clean
clean:
	rm -r $(BUILD_DIR)/*.o
	rm -r *.x
# ======================================================================
