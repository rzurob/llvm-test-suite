#!/bin/ksh

echo 'Compiling files'
echo ${COMPILER} ${TR_SRC}/$1.f ${OPTIONS}
${COMPILER} ${TR_SRC}/$1.f ${OPTIONS}
RC=$?

if [ $RC = 0 ]
then
  echo 'Compile File file successful'
else
  echo 'Compile File file failed'; exit $RC
fi

./a.out >$1.outs
xldiff $1.outs ${TR_SRC}/$1.vf
RC=$?

if [ $RC = 0 ]
then
  echo 'Verification successful'
else
  echo 'Verification failed'; exit $RC
fi

rm -f $1.outs a.out $1.o
exit 0
