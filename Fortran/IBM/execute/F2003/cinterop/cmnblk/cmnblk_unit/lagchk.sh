#!/bin/ksh

echo "${OPTIONS}" |grep smp>/dev/null
RC=$?
if [ $RC = 0 ]
then
  VF=$1.vf2
else 
  VF=$1.vf1
fi

echo 'Compiling files ..'
echo ${COMPILER} ${TR_SRC}/$1.f ${OPTIONS} 2>$1.err1
${COMPILER} ${TR_SRC}/$1.f ${OPTIONS} 2>$1.err1
RC=$?

echo ""
echo "Sorting err file ..."
echo "sort ./$1.err1 > ./$1.err"
sort ./$1.err1 > ./$1.err

echo ""
echo "Sorting verification file ..."
echo "sort ${TR_SRC}/$VF > ./$1.vf"
sort ${TR_SRC}/$VF > ./$1.vf


echo "xldiff $1.err $1.vf"
xldiff $1.err $1.vf
RC=$?

if [ $RC = 0 ]
then
  echo 'Verification successful'
else
  echo 'Verification failed'; exit $RC
fi

rm -f $1.err* mod.mod a.out $1.vf
exit 0
