#!/bin/ksh

echo "${OPTIONS}" |grep O4 >/dev/null
RC=$?
if [ $RC = 0 ]
then
  VF=$1.vf2
else
  echo "${OPTIONS}" |grep O5 >/dev/null
  RC=$?
  if [ $RC = 0 ]
  then
    VF=$1.vf2
  else
    VF=$1.vf1
  fi
fi

echo 'Compiling files'
echo ${COMPILER} ${TR_SRC}/$1.f ${OPTIONS} 2>$1.err
${COMPILER} ${TR_SRC}/$1.f ${OPTIONS} 2>$1.err

sort $1.err >$1.sort
xldiff $1.sort ${TR_SRC}/$VF
RC=$?

if [ $RC = 0 ]
then
  echo 'Verification successful'
else
  echo 'Verification failed'; exit $RC
fi

rm -f $1.err $1.sort $1.o mod.mod a.out
exit 0
