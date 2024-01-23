#!/bin/ksh

if [ $# = 2 ] 
then
  echo "${OPTIONS}" |grep extchk >/dev/null
  RC=$?
  if [ $RC = 0 ]
  then
    OPT1="-qextchk" 
  fi

  echo "${OPTIONS}" |grep q64 >/dev/null
  RC=$?
  if [ $RC = 0 ]
  then
    OPT2="-q64" 
  fi

  echo 'Compiling C files'
  echo 'xlc -c' ${TR_SRC}/$2.c $OPT1 $OPT2
  xlc -c ${TR_SRC}/$2.c $OPT1 $OPT2
  RC=$?

  if [ $RC = 0 ]
  then
    echo 'Compile C file successful'
  else
    echo 'Compile C file failed'; exit $RC
  fi
  
  CSUBOBJ=$2.o
fi

echo 'Compiling Fortran files'
echo ${COMPILER} ${TR_SRC}/$1.f $CSUBOBJ ${OPTIONS} 2>$1.outs
${COMPILER} ${TR_SRC}/$1.f $CSUBOBJ ${OPTIONS} 2>$1.outs

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
