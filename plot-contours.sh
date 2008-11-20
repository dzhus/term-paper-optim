#!/bin/sh
#
# Wrap contours plotted with gnuplot in TikZ commands
#
# Usage:
#
#     ./plot-contours.sh FUNCTION
#
# Output is a set of TikZ `\draw` commands for each contour produced
# by FUNCTION.gp. See also `contour-path.tpl.tkz.tex` template.

FUNCTION=$1

rm -fr ${FUNCTION}-contours-*

gnuplot ${FUNCTION}.gp

for contour in ${FUNCTION}-contours-*
do
    level=$(echo "${contour}" | cut -d- -f 3)
    m4 --define="__LEVEL"=${level} \
        --define="__FILE"=${contour} \
        contour-path.tpl.tkz.tex
done
