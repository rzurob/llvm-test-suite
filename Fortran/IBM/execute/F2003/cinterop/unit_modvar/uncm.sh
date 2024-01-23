#!/bin/ksh

echo "${OPTIONS}" |grep q64 >/dev/null
RC=$?
if [ $RC = 0 ]
then
  XLCOPT="-q64" 
fi

echo 'Compiling C files'
echo xlc -c ${TR_SRC}/$2.c $XLCOPT
xlc -c ${TR_SRC}/$2.c $XLCOPT
RC=$?
if [ $RC = 0 ]
then
  echo 'Compile file successful'
else
  echo 'Compile file failed'; exit $RC
fi


echo 'Compiling Fortran files'
echo ${COMPILER} ${TR_SRC}/$1.f ${OPTIONS} $2.o 
${COMPILER} ${TR_SRC}/$1.f ${OPTIONS} $2.o
RC=$?

if [ $RC = 0 ]
then
  echo 'Compile Fortran file successful'
else
  echo 'Compile Fortran file failed'; exit $RC
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

rm -f $1.outs mod.mod $1.o a.out
exit 0
