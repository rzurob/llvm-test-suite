#!/bin/ksh
# Test commmand line length limit


function cleanup
{
  if [ "$TRUN_SAVE" != "yes" ]; then

    rm -f core $1 $1.o* *.mod a.out fxclms050 

 fi
}


echo  "Compiling the program ..."
echo "${KILL} xlc ${TR_SRC}/fxclms05.c"
${KILL} xlc ${TR_SRC}/fxclms05.c 
rc=$?

echo "${KILL} ${COMPILER}  ${TR_SRC}/fxclms050.f  ${OPTIONS} -o fxclms050"
${KILL} ${COMPILER}  ${TR_SRC}/fxclms050.f  ${OPTIONS} -o fxclms050

let rc=rc+$?

if [ $rc -ne 0 ]; then
  echo "Compilation Failed"
  cleanup $1
  exit $rc
else
  echo "Compilation Successful"
fi

#  0)  OS="AIX"
#  1)  OS="LINUX"
#  2)  OS="MACOSX"
echo "Executing the program ..."

echo "./a.out `isAIX`"

./a.out `isAIX` 

rc=$?

if [ $rc -ne 0 ]; then
  echo "Execution Failed"
else
  echo "Execution Successful"
fi

cleanup $1
exit $rc
