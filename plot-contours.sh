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

# Remove old contours (possibly splitted)
rm -fr ${FUNCTION}-contour-*

gnuplot ${FUNCTION}-contours.gp

for contour in ${FUNCTION}-contour-*
do
    # Split every contour
    level=$(echo "${contour}" | cut -d- -f 3)
    
    # Remove comments
    tmp=$(mktemp /tmp/docXXXXXX)
    grep -v '#.*' "${contour}" > ${tmp}

    # Split contour to connected subparts
    csplit -f ${contour} -b '-%02d' -qz ${tmp} "/^$/" "{*}"
    for part in ${contour}-*
    do
        m4 --define="__LEVEL"=${level} \
            --define="__FILE"=${part} \
            contour-path.tpl.tkz.tex
    done
done
