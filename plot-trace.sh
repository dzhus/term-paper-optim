#!/bin/sh
#
# Produce a TikZ command which draws an optimization trace from given
# file
#
# Usage:
#
#     ./plot-trace.sh FILE-PREFIX
#
# Output is a single TikZ `\draw` command which will draw path from
# `FILE-PREFIX-trace` file. See also `trace-path.tpl.tkz.tex`
# template.

PREFIX=$1

m4 --define="__FILE"=${PREFIX}-trace \
    trace-path.tpl.tkz.tex
