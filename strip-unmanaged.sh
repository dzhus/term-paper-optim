#!/bin/sh

# Miscellaneous script which filters out names of files which are not
# managed by Mercurial
#
# Limitation: won't filter out unmanaged `foo.txt` in current
# directory if there is managed `blah-blah/foo.txt` somewhere in child
# directories.

manifest=$(mktemp /tmp/XXXXX)
hg manifest > ${manifest}
for file in $*
do
    if grep "${file}" ${manifest} > /dev/null
    then
        echo -n "${file} "
    fi
done

rm ${manifest}
