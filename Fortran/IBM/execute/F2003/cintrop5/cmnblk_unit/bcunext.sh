#!/bin/ksh

COMPILER=`expr $COMPILER : '\(.*/\)'`xlf90_r
echo 'Compiling files'
echo ${COMPILER} ${TR_SRC}/$1.f ${OPTIONS}
${COMPILER} ${TR_SRC}/$1.f ${OPTIONS}
RC=$?
if [ $RC = 0 ]
then
  echo 'Compile successful'
else
  echo 'Compile failed'; exit $RC
fi

awk '/SOURCE SECTION/, /OBJECT SECTION/' $1.lst>$1.attrlst
diff $1.attrlst ${TR_SRC}/$1.vf
RC=$?

if [ $RC = 0 ]
then
  echo 'Verification successful'
else
  echo 'Verification failed'; exit $RC
fi

rm -f $1.attrlst $1.lst a.out
exit 0