# ======================================================================
# Makefile : GPECN1DTDPBC
#
# LAST UPDATED
#
#     April, 13th, 2021
#
# ----------------------------------------------------------------------
#
#    Specify the SHELL that will interpret this Makefile. This line avoids
#    issues on systems where the SHELL variable might be inherited from
#    the environment. 

SHELL := /bin/bash

#    Get hostname of the machine the source will be compiled and run on.

HOSTNAME := $(shell hostname)

#    Specify general user-defined compilation options.

COMPILER     := gfortran
OPTIMIZATION := ON
OPENMP       := OFF
DEBUG        := OFF
PROFILE      := OFF

#    Set compiler-specific options.

ifeq ($(COMPILER),gfortran)

   STANDARD_OPTIONS     := -fimplicit-none -fmodule-private \
                           -ffree-form -ffree-line-length-none -std=gnu
   INTEGER_OPTIONS      := -fdefault-integer-8
   REAL_OPTIONS         := -fdefault-real-8 -fdefault-double-8
   OPTIMIZATION_OPTIONS := -O3 -ffast-math -mtune=native
   OPENMP_OPTIONS       := -fopenmp
   CHECK_OPTIONS        := -fcheck=all
   DEBUG_OPTIONS        := -ffpe-trap=invalid,overflow -fbacktrace \
                           -fdump-core -finit-real=nan
   WARNING_OPTIONS      := -Wall -fmax-errors=0 -Wno-array-temporaries \
                           -Warray-bounds -Wcharacter-truncation \
                           -Wline-truncation -Wconversion-extra \
                           -Wimplicit-interface -Wimplicit-procedure \
                           -Wunderflow -Wextra -Wuninitialized
   PROFILE_OPTIONS      := -pg

endif

ifeq ($(COMPILER),ifort)

   STANDARD_OPTIONS     := -implicitnone -free -stand none -module build
   INTEGER_OPTIONS      := -integer-size 64
   REAL_OPTIONS         := -real-size 64 -double-size 64
   OPTIMIZATION_OPTIONS := -O3 -fast -mtune=native
   OPENMP_OPTIONS       := -qopenmp
   CHECK_OPTIONS        := -check all
   DEBUG_OPTIONS        := -debug all
   WARNING_OPTIONS      := -warn all
   PROFILE_OPTIONS      := -pg

endif

ifeq ($(COMPILER),flang)

   STANDARD_OPTIONS     := -Mfreeform -Mstandard
   INTEGER_OPTIONS      := -fdefault-integer-8
   REAL_OPTIONS         := -fdefault-real-8
   OPTIMIZATION_OPTIONS := -O3 -ffast-math -mtune=native
   OPENMP_OPTIONS       := -mp 
   CHECK_OPTIONS        :=
   DEBUG_OPTIONS        :=
   WARNING_OPTIONS      := -Weverything
   PROFILE_OPTIONS      := -pg

endif

COMPILER_OPTIONS := $(STANDARD_OPTIONS) $(INTEGER_OPTIONS) \
                    $(REAL_OPTIONS)

ifeq ($(DEBUG),ON)

   COMPILER_OPTIONS += -O0 -C -g $(CHECK_OPTIONS) $(DEBUG_OPTIONS) \
                       $(WARNING_OPTIONS)

endif

ifeq ($(PROFILE),ON)

   COMPILER_OPTIONS += $(PROFILE_OPTIONS)

endif

ifeq ($(OPTIMIZATION),ON)

   COMPILER_OPTIONS += $(OPTIMIZATION_OPTIONS)

endif

ifeq ($(OPENMP),ON)

   COMPILER_OPTIONS += $(OPENMP_OPTIONS)

endif


SOURCE_DIR := source
LAPACK_DIR := libraries/lapack
BUILD_DIR := build
FIGURE_DIR := figures

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
