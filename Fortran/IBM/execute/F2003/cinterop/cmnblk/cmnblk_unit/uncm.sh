#!/bin/ksh

echo "${OPTIONS}" |grep q64 >/dev/null
RC=$?
if [ $RC = 0 ]
then
  XLCOPT="-q64" 
fi

echo 'Compiling C files'
echo xlc ${TR_SRC}/$2.c $XLCOPT -c
xlc ${TR_SRC}/$2.c $XLCOPT -c
RC=$?

if [ $RC = 0 ]
then
  echo 'Compile C file successful'
else
  echo 'Compile C file failed'; exit $RC
fi


echo 'Compiling Fortran and linking with C files'
echo ${COMPILER} ${TR_SRC}/$1.f ./$2.o ${OPTIONS} -o $1
${COMPILER} ${TR_SRC}/$1.f ./$2.o ${OPTIONS} -o $1
RC=$?

if [ $RC = 0 ]
then
  echo 'Compile Fortran and linking with C files successful'
else
  echo 'Compile Fortran and linking with C files failed'; exit $RC
fi

./$1 >$1.outs
xldiff $1.outs ${TR_SRC}/$1.vf
RC=$?

if [ $RC = 0 ]
then
  echo 'Verification successful'
else
  echo 'Verification failed'; exit $RC
fi

rm -f $1.outs mod.mod $1.o $1 $2.o
exit 0
