#!/bin/ksh

echo 'Compiling files'
echo ${COMPILER} ${TR_SRC}/$1.f ${OPTIONS} 2>$1.err
${COMPILER} ${TR_SRC}/$1.f ${OPTIONS} 2>$1.err
RC=$?

xldiff $1.err ${TR_SRC}/$1.vf
RC=$?

if [ $RC = 0 ]
then
  echo 'Verification successful'
else
  echo 'Verification failed'; exit $RC
fi

rm -f $1.err $1.lst *.mod a.out
exit 0
