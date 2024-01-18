#!/bin/ksh
# Test a commmand composed of multiple lines 


function cleanup
{
  if [ "$TRUN_SAVE" != "yes" ]; then

    rm -f core fxclms19 

 fi
}


echo  "Compiling the program ..."

echo "${KILL} ${COMPILER}  ${TR_SRC}/fxclms19.f ${OPTIONS} -o fxclms19"
${KILL} ${COMPILER}  ${TR_SRC}/fxclms19.f ${OPTIONS} -o fxclms19

rc=$?

if [ $rc -ne 0 ]; then
  echo "Compilation Failed"
  cleanup 
  exit $rc
else
  echo "Compilation Successful"
fi

echo "Executing the program ..."
echo "fxclms19 012... --... ++... ==..."

fxclms19 \
012345678901234567890123456789012345678901234567890123456789 \
------------------------------------------------------------ \
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ \
============================================================ \
____________________________________________________________


rc=$?

if [ $rc -ne 0 ]; then
  echo "Execution Failed"
else
  echo "Execution Successful"
fi

cleanup 
exit $rc

