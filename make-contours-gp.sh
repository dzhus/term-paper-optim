#!/bin/bash
#
# Given a `.setup` file, write a set of Gnuplot commands to plot
# contours for surface
#
# Usage:
#
#     ./make-contours.sh <id>-contours.setup
#
# `.setup` file is sourced by shell and must set the following
# variables: `F` (gnuplot-compliant definition of binary function),
# `X_MIN`, `X_MAX`, `Y_MIN`, `Y_MAX`, `LEVELS` (an array containg a
# set of numbers in ascending order each corresponding to one surface
# level to be plotted), `ISOSAMPLES`. It may optionally set `PREAMBLE`
# (gnuplot commands to be executed first) and `SAMPLES` variables.

# Default values
ISOSAMPLES=10
SAMPLES=100

# . needs to be in PATH for this to work when running bash in POSIX
# mode
source $1

# extract `rosenbrock` from `rosenbrock-contours.setup`
NAME=$(echo $1 | cut -d- -f 1)

for l in ${LEVELS[@]}
do
    CONTOURS="${CONTOURS}
"$(m4 --define="__LEVEL"="${l}" \
    --define="__NAME"="${NAME}" \
    --define="__X_MIN"="${X_MIN}" \
    --define="__X_MAX"="${X_MAX}" \
    --define="__Y_MIN"="${Y_MIN}" \
    --define="__Y_MAX"="${Y_MAX}" \
    contour-level.tpl.gp)
done

m4 --define="__F"="${F}" \
    --define="__PREAMBLE"="${PREAMBLE}" \
    --define="__ISOSAMPLES"="${ISOSAMPLES}" \
    --define="__SAMPLES"="${SAMPLES}" \
    --define="__CONTOURS"="${CONTOURS}" \
    contours.tpl.gp
