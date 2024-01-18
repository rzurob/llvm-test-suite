#!/bin/ksh
$COMPILER -o $1 $TR_SRC/$1.f $OPTIONS

if [ $? -ne 0 ]
then
   echo "Compilation Failed!"
   exit 1
fi

$1 2> $1.err
rc=$?
if [ $rc -eq 0 ] ; then
   diff $1.err $TR_SRC/$1.vf
   rc=$?
   if [ $rc -ne 0 ]
   then
       echo "Verification Failed!"
       exit $rc
   else
       rm -f $1.err $1
   fi
else
       echo "Execution Failed!"
       exit $rc
fi   
