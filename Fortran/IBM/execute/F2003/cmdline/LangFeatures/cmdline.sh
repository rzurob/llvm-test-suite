#!/bin/ksh

export KILL=/xlftest/bin/trkill
function cleanup
{
  if [ "$TRUN_SAVE" != "yes" ]; then

    rm -f core $1 $1.o* *.mod 

  fi
}

echo  "Compiling the program ..."
echo "${COMPILER}  -o $1 ${TR_SRC}/$1.f" ${OPTIONS}
${KILL} ${COMPILER} -o $1 ${TR_SRC}/$1.f ${OPTIONS} 
rc=$?

if [ $rc -ne 0 ]; then
  echo "Compilation Failed"
  cleanup $1
  exit $rc
else
  echo "Compilation Successful"
fi

echo "Executing the program ..."

# with various opts/chars
#CmdLine defined in PRECMD

echo "${CmdLine}"
${KILL} ${CmdLine}
rc=$?

if [ $rc -ne 0 ]; then
  echo "Execution Failed"
else
  echo "Execution Successful"
fi

cleanup $1
exit $rc

