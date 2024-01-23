#!/bin/ksh

echo "${OPTIONS}" |grep qsmp >/dev/null
RC=$?
if [ $RC = 0 ]
then
  VF=$1.vf2
else 
  VF=$1.vf1
fi

echo 'Compiling files'
echo ${COMPILER} ${TR_SRC}/$1.f ${OPTIONS} 2>$1.err
${COMPILER} ${TR_SRC}/$1.f ${OPTIONS} 2>$1.err
RC=$?

xldiff $1.err ${TR_SRC}/$VF
RC=$?

if [ $RC = 0 ]
then
  echo 'Verification successful'
else
  echo 'Verification failed'; exit $RC
fi

rm -f $1.err mod.mod a.out
exit 0
