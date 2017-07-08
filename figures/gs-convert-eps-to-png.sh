#!/usr/bin/env bash
# ======================================================================                                                                                             
# NAME
# 
#     gs-convert-eps-to-png.sh
#
# DESCRIPTION
#
#     A simple bash script to convert an eps file to png using 
#     Ghostscript (gs). The script is intended to be used to convert the 
#     eps files output by freqtime.gpi and spacetime.gpi to png, which 
#     creates much smaller file sizes that may be more easily included 
#     as figures in a LaTeX document.
#
# USAGE
#
#     gnuplot spacetime.gpi
#     ./gs-convert-eps-to-png.sh spacetime
#
# AUTHOR
#
#     Marty Kandes, Ph.D.
#     Computational & Data Science Research Specialist
#     User Services Group
#     San Diego Supercomputer Center
#     University of California, San Diego
#
# COPYRIGHT
#     
#     Copyright (c) 2017 Martin Charles Kandes
#
# LICENSE
#
#     The MIT License (MIT)
#
# LAST UPDATED
#
#     Friday, July 7th, 2017
#
# ----------------------------------------------------------------------

gs -r300 -dEPSCrop -dTextAlphaBits=4 -sDEVICE=png16m \
   -sOutputFile="${1}".png -dBATCH -dNOPAUSE "${1}".eps
