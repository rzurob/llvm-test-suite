#!/bin/ksh

echo 'Compiling files'
echo ${COMPILER} ${TR_SRC}/$1.f ${OPTIONS}
${COMPILER} ${TR_SRC}/$1.f ${OPTIONS}
RC=$?

if [ $RC = 0 ]
then
  echo 'Compilation done'
else
  echo 'Compilation failed'; exit $RC
fi

awk '/IDENTIFIER NAME/,/End of Compilation/' $1.lst >$1.attrlst
echo xldiff $1.attrlst ${TR_SRC}/$1.vf
xldiff $1.attrlst ${TR_SRC}/$1.vf
RC=$?

if [ $RC = 0 ]
then
  echo 'Verification list file successful'
else
  echo 'Verification list file failed'; exit $RC
fi

rm -f $1.lst $1.attrlst a.out mod.mod

exit 0
