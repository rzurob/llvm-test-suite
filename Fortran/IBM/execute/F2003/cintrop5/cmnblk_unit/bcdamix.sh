#!/bin/ksh

echo 'Compiling files'
echo ${COMPILER} ${TR_SRC}/$1.f ${OPTIONS} 2>$1.err
${COMPILER} ${TR_SRC}/$1.f ${OPTIONS} 2>$1.err
#${COMPILER} $1.f ${OPTIONS} 2>$1.vf1
RC=$?

xldiff $1.err ${TR_SRC}/$1.vf1
RC=$?

if [ $RC = 0 ]
then
  echo 'Verification successful'
else
  echo 'Verification failed'; exit $RC
fi

rm -f $1.err $1.lst mod.mod a.out

echo 'Compiling files with -qmixed'
echo ${COMPILER} ${TR_SRC}/$1.f ${OPTIONS} -qmixed 2>$1.err
${COMPILER} ${TR_SRC}/$1.f ${OPTIONS} -qmixed 2>$1.err
#${COMPILER} $1.f ${OPTIONS} -qmixed 2>$1.vf2
RC=$?

xldiff $1.err ${TR_SRC}/$1.vf2
RC=$?

if [ $RC = 0 ]
then
  echo 'Verification successful'
else
  echo 'Verification failed'; exit $RC
fi

rm -f $1.err $1.lst mod.mod a.out
exit 0