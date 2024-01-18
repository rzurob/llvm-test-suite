#!/bin/ksh

echo 'Compiling files'
echo ${COMPILER} ${TR_SRC}/$1.f ${OPTIONS} 2>$1.err
${COMPILER} ${TR_SRC}/$1.f ${OPTIONS} 2>$1.err
#${COMPILER} $1.f ${OPTIONS} 2>$1.vf
RC=$?

xldiff $1.err ${TR_SRC}/$1.vf
RC=$?

if [ $RC = 0 ]
then
  echo 'Verification successful'
else
  echo 'Verification failed'; exit $RC
fi

rm -f $1.err mod.mod $1.o a.out
exit 0
