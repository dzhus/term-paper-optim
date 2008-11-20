#!/bin/sh
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
# `X_MIN`, `X_MAX`, `Y_MIN`, `Y_MAX`, `LEVELS` (a string containg a
# set of space-separated numbers each corresponding to one surface
# level to be plotted), `ISOSAMPLES`.

source $1

for l in ${LEVELS}
do
    CONTOURS="${CONTOURS}
"$(m4 --define="__LEVEL"="${l}" \
    --define="__X_MIN"="${X_MIN}" \
    --define="__X_MAX"="${X_MAX}" \
    --define="__Y_MIN"="${Y_MIN}" \
    --define="__Y_MAX"="${Y_MAX}" \
    contour-level.tpl.gp)
done

m4 --define="__F"="${F}" \
    --define="__ISOSAMPLES"="${ISOSAMPLES}" \
    --define="__CONTOURS"="${CONTOURS}" \
    contours.tpl.gp