#!/bin/bash
# ======================================================================
# NAME
#
#   gpecn1Dtdpbc.sh
#
# SYNOPSIS
#
#   make
#   ./gpecn1Dtdpbc.sh
#
# DESCRIPTION
#
#   A bash script to run gpecn1Dtdpbc.x and its supporting programs.
#
# NOTES
#
#   All dimensional quantities defined here are assumed to be in terms 
#   of simple harmonic oscillator units.
#
# LAST UPDATED
#
#   Thursday, March 30th, 2017
#
# ----------------------------------------------------------------------

# Define total number grid points on the one-dimensional ring (S1).
declare -r number_of_grid_points=1256

# Define total number of time steps in simulation.
declare -r number_of_time_steps=80000

# Define the number of time steps to skip between writing output to disk.
declare -r number_of_time_steps_between_writes=80

# Define the number of Crank-Nicolson iterations performed per time step.
declare -r number_of_iterations_per_time_step=1

# Always define the gamma_factor >= 1.0. This parameter sets the CFL 
# condition for the simulation to maintain stability and prevent 
# suprious oscillations from arising in the solutions.
declare -r gamma_factor=1.0

# Define initial state wave function. 
#
#   wave_function_switch=0
#
#     wavefunc.x generates a (Gaussian) ground state solution of a 
#     one-dimensional simple harmonic oscillator (SHO) potential and 
#     places it into a coherent superposition of two momentum states to 
#     create two propagating Gaussian wave packets.
#
#   wave_function_switch=1
#
#     wavefunc.x generates a superposition of two quantized plane wave 
#     solutions for a one-dimensional ring.
#
declare -r wave_function_switch=0

# Define external potential acting on wave function. 
#
#   external_potential_switch=0
#
#     potential.x generates a zero potential for all points on the ring.
#
#   external_potential_switch=1
#
#     potential.x generates a one-dimensional simple harmonic oscillator
#     potential centered around an angular position on the ring.
#
declare -r external_potential_switch=0

# Define nonlinear coupling.
#
#   nonlinear_coupling_switch=0
#
#     nonlinear.x generates a uniform, time-independent, and real-valued
#     coupling constant that parameterizes the strength of a (simple, 
#     single-component) Bose-Einstein condensate’s nonlinear mean-field
#     interaction arising from two-body contact-like interactions 
#     between atoms within the condensate.
#
#     nonlinear.x was originally intented to explore many different
#     spatially-complex nonlinear couplings, some even with imaginary
#     components. However, this work was not pursused. As such, no other 
#     options for nonlinear_coupling_switch are currently available.
#
declare -r nonlinear_coupling_switch=0

# Set real/imaginary time mode.
#
#   imaginary_time_switch=0
#
#     Always set imaginary_time_switch=0. gpecn1Dtdpbc.x was originally
#     intended to support impaginary time propagating (ITP) to generate
#     initial state wave function solutions. However, this feature was 
#     never implemented. Only analytic initial state wave functions may
#     be used.
#
declare -r imaginary_time_switch=0

# Define the radius of the ring
declare -r radius_of_ring=10.0

# Define oscillator strength of Gaussian wave packets and/or simple 
# harmonic oscillator potential.
declare -r gaussian_oscillator_strength=0.1

# Define initial angular position of initial wave packets generated by 
# wave_function_switch=0 and/or simple harmonic oscillator potential 
# created by external_potential_switch=1.
declare -r initial_angular_position=1.570796326794896619231321691639751442098584699687552910487

# Define amplitude of first component in the two momentum state 
# superpositions created by wavefunc.x's wave_function_switch=0,1 
# options. The amplitude of the second component is simply defined as 
# the complement of amplitude_1 to conserve (unit) probability. i.e., 
# amplitude_2=sqrt(1-amplitude_1^2). Therefore, for a symmetric 
# superposition set amplitude_1 = 1/sqrt(2).
declare -r amplitude_1=0.707106781186547524400844362104849039284835937688474036588

# Define initial phases associated with the two quantized plane wave 
# solutions in the superposition created by wave_function_switch=1.
declare -r initial_phase_1=0.0
declare -r initial_phase_2=0.0

# Define initial angular momenta imparted to the two momentum states 
# created in wavefunc.x's wave_function_switch=0,1 options.
declare -r angular_momentum_1=10
declare -r angular_momentum_2=-10

# Define carrier frequency analyzed by ftpsd's Fourier transform phase 
# shift determination algorithm. See K. A. Goldberg and J. Bokor. 
# Fourier-Transform Method of Phase-Shift Determination. Applied Optics,
# 40(17):2886–2894, 2001. 
declare -r carrier_frequency="$(echo ${angular_momentum_1} - ${angular_momentum_2} | bc)"

# Define strength of nonlinear mean-field interaction.
declare -r nonlinear_coupling_amplitude=0.0

# Define angular velocity of the rotating one-dimensional ring.
declare -r rotation_rate_of_ring=0.001

# Compute initial state wave function.
./wavefunc.x "${number_of_grid_points}" \
             "${wave_function_switch}" \
             "${radius_of_ring}" \
             "${rotation_rate_of_ring}" \
             "${gaussian_oscillator_strength}" \
             "${initial_angular_position}" \
             "${amplitude_1}" \
             "${angular_momentum_1}" \
             "${angular_momentum_2}" \
             "${initial_phase_1}" \
             "${initial_phase_2}" > wavefunc.output

# Compute time-independent external potential.
./potential.x "${number_of_grid_points}" \
              "${external_potential_switch}" \
              "${radius_of_ring}" \
              "${gaussian_oscillator_strength}" \
              "${initial_angular_position}" > potential.output

# Compute nonlinear, mean-field potential.
./nonlinear.x "${number_of_grid_points}" \
              "${nonlinear_coupling_switch}" \
              "${radius_of_ring}" \
              "${nonlinear_coupling_amplitude}" > nonlinear.output

# Simulate.
./gpecn1Dtdpbc.x "${number_of_grid_points}" \
                 "${number_of_time_steps}" \
                 "${number_of_time_steps_between_writes}" \
                 "${number_of_iterations_per_time_step}" \
                 "${imaginary_time_switch}" \
                 "${radius_of_ring}" \
                 "${rotation_rate_of_ring}" \
                 "${gamma_factor}" > gpecn1Dtdpbc.output

# Compute expectation values.
./expectation.x "${number_of_grid_points}" \
                "${number_of_time_steps}" \
                "${number_of_time_steps_between_writes}" \
                "${radius_of_ring}" \
                "${rotation_rate_of_ring}" \
                "${gamma_factor}" > expectation.output

# Compute phase shifts from interference. 
start_file=1000
end_file="$(echo ${start_file}+${number_of_time_steps}/${number_of_time_steps_between_writes} | bc )"
./ftpsd.x "${number_of_grid_points}" \
          "${number_of_time_steps_between_writes}" \
          "${start_file}" \
          "${end_file}" \
          "${radius_of_ring}" \
          "${gamma_factor}" \
          "${carrier_frequency}" > ftpsd.output

# Convert binary wave function file to write out its (probability) 
# density, real, and imaginary components in ascii format.
current_file="${start_file}"
output_shift=9000
while [[ "${current_file}" -le "${end_file}" ]]; do
  ./binary.x "${number_of_grid_points}" \
             "${current_file}" \
             "${output_shift}" \
             "${radius_of_ring}" > binary.output
  ((current_file+=1))
done

# Compute momentum distribution.
current_file="${start_file}"
output_shift=19000
minimum_momentum=-100
maximum_momentum=100
while [ "${current_file}" -le "${end_file}" ]; do
  ./momentum.x "${number_of_grid_points}" \
               "${current_file}" \
               "${output_shift}" \
               "${minimum_momentum}" \
               "${maximum_momentum}" \
               "${radius_of_ring}" > momentum.output
  ((current_file+=1))
done

# Create space-time data.
output_file=591
./spacetime.x "${number_of_grid_points}" \
              "${number_of_time_steps}" \
              "${number_of_time_steps_between_writes}" \
              "${start_file}" \
              "${end_file}" \
              "${output_file}" \
              "${radius_of_ring}" \
              "${gamma_factor}"

# Create freq-time data.
output_file=592
./freqtime.x "${number_of_grid_points}" \
             "${number_of_time_steps}" \
             "${number_of_time_steps_between_writes}" \
             "${start_file}" \
             "${end_file}" \
             "${output_file}" \
             "${minimum_momentum}" \
             "${maximum_momentum}" \
             "${radius_of_ring}" \
             "${gamma_factor}"

# ======================================================================