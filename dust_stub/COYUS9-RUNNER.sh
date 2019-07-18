#!/bin/sh

MYDIR=`readlink -f $(dirname $0)/`

COYU9DAT=$1

if [ -z "$COYU9DAT" ]; then
    echo "Give me a COYU9.DAT file as an argument"
    exit 1
fi

if [ ! -r $COYU9DAT ]; then
    echo "Input file $COYU9DAT is not readable" 2>&1
    exit 1
fi

echo "Running with $COYU9DAT"

Rscript  --default-packages=datasets,utils,grDevices,graphics,stats,methods --vanilla "$MYDIR/COYUsRunner.R" "$COYU9DAT" "VBERRORS.DAT"

