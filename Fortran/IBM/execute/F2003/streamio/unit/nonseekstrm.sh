#!/bin/ksh
$COMPILER -o $1 $TR_SRC/$1.f $OPTIONS

if [ $? -ne 0 ]
then
   echo "Compilation Failed!"
   exit 1
fi

$1 

rc=$?

if [ $rc -ne 0 ] ; then
    echo "Execution failed!"
    exit $rc 
else
    rm -f $1
fi
