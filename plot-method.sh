#!/bin/sh
#
# Wrap contours plotted with gnuplot in TikZ environment.
#
# Usage:
#
#     ./plot-method.sh FUNCTION
#
# Output is LaTeX `figure` environment containing `tikzpicture` with
# contours from file FUNCTION.gp

FUNCTION=$1
METHOD=$2

for contour in ${FUNCTION}-contours-*.out
do
    level=$(echo "${contour}" | sed -e "s/.*\-\(.*\)\.out/\\1/")
    CONTOURS="${CONTOURS}
"$(m4 --define="__LEVEL"=${level} \
      --define="__FILE"=${contour} \
    contour-path.tpl.tkz.tex)
done

m4 --define="__CONTOURS"="${CONTOURS}" \
    plot-method.tpl.tkz.tex
