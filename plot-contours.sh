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
#
# Limitation: does not work with negative level contours

FUNCTION=$1

# Get maximum level to calculate color scale factor later
source ${FUNCTION}-contours.setup
max_level=${LEVELS[$(( ${#LEVELS[@]} - 1))]}

# Remove old contours (possibly splitted)
rm -fr ${FUNCTION}-contour-*

gnuplot ${FUNCTION}-contours.gp

# Prepend style with comma, if it is set
if [ "${STYLE}" = "" ]
then
    style=""
else
    style=",${STYLE}"
fi

for contour in ${FUNCTION}-contour-*
do
    # Get current level
    level=$(echo "${contour}" | cut -d- -f 3)

#     # Calculate scale of current level, avoiding too small scales
#     s_level=$(echo "if (${level} >= sqrt(${max_level})) ${level} else sqrt(${max_level})" | bc)
#     scale=$(echo "scale=3; l(${s_level}+1)/l(${max_level}+1)*100" | bc -l)
    scale="100"

    # Remove comments from point lists
    tmp=$(mktemp /tmp/docXXXXXX)
    grep -v '#.*' "${contour}" > ${tmp}

    # Split contour to connected subparts
    csplit -f ${contour} -b '-%02d' -qz ${tmp} "/^$/" "{*}"
    for part in ${contour}-*
    do
        # Removing empty subparts
        if grep -v '^#* *$' $part > /dev/null
        then
        m4 --define="__LEVEL"=${level} \
            --define="__FILE"=${part} \
            --define="__STYLE"="${style}" \
            --define="__COLORSCALE"=${scale} \
            contour-path.tpl.tkz.tex
        else
            rm ${part}
        fi
    done
done
