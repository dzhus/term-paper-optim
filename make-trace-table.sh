#!/bin/sh
#
# Usage:
#
#     ./make-trace-table.sh FILE-PREFIX
#
# Output is `\pgfplotstabletypesetfile` command which will typeset
# number from `FILE-PREFIX-trace` file. See also `trace-table.tpl.tex`
# template.
#
# First N columns, where N is an arity of traced function, get `$x_1$,
# $x_2$, …` headerlabels. See also `trace-table-var-colname.tpl`
# tempate.

NON_VAR_FIELDS=1

prefix=$1
file="${prefix}-trace"

total_fields=$(cat ${file} | head -n 1 | awk '{ print NF }')
arity=$(( total_fields - NON_VAR_FIELDS))

for index in $(seq ${arity})
do
    new_column=$(m4 --define="__INDEX"="$index"\
                    --define="__SINDEX"="$(( index - 1 ))"\
                    trace-table-var-colname.tpl)
    col_names="${col_names} ${new_column}"
done

m4 --define="__COLUMNS"="${col_names}" \
    --define="__FILE"="${file}" \
    trace-table.tpl.tex
