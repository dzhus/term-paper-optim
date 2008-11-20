#!/bin/sh
#
# Produce a TikZ command which draws a path tracing optimization of
# given function
#
# Usage:
#
#     ./plot-trace.sh FUNCTION
#
# Output is a single TikZ `\draw` command. See also
# `trace-path.tpl.tkz.tex` template.

FUNCTION=$1

m4 --define="__FILE"=${FUNCTION}-trace \
    trace-path.tpl.tkz.tex
