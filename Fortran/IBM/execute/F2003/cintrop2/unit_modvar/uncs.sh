#!/bin/ksh

if [ $# = 2 ] 
then
  echo "${OPTIONS}" |grep q64 >/dev/null
  RC=$?
  if [ $RC = 0 ]
  then
    XLCOPT="-q64" 
  fi

  echo 'Compiling C files'
  echo 'xlc -c' ${TR_SRC}/$2.c $XLCOPT
  xlc -c ${TR_SRC}/$2.c $XLCOPT
  RC=$?

  if [ $RC = 0 ]
  then
    echo 'Compile C file successful'
  else
    echo 'Compile C file failed'; exit $RC
  fi
  
  CSUBOBJ=$2.o
else
  if [ $# = 1 ]
  then
    CSUBOBJ=''
  else
    exit 1
  fi
fi

echo 'Compiling Fortran files'
echo ${COMPILER} ${TR_SRC}/$1.f $CSUBOBJ ${OPTIONS}
${COMPILER} ${TR_SRC}/$1.f $CSUBOBJ ${OPTIONS}
RC=$?

if [ $RC = 0 ]
then
  echo 'Compile file successful'
else
  echo 'Compile file failed'; exit $RC
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

rm -f $1.outs mod.mod $2.o a.out
exit 0