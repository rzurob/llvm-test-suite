#!/bin/sh
echo ${OPTIONS} | grep \\-q64
if [ $? -eq 0 ]
then echo {COMPILER} -WF,-DBITMODE64 ${TR_SRC}/iccons.F -o iccons ${OPTIONS}
  ${COMPILER} -WF,-DBITMODE64 ${TR_SRC}/iccons.F -o iccons ${OPTIONS}
else echo ${COMPILER} ${TR_SRC}/iccons.F -o iccons ${OPTIONS}
  ${COMPILER} ${TR_SRC}/iccons.F -o iccons ${OPTIONS}
fi
echo ./iccons
./iccons
RETVAL=$?
echo rm -f iccons
rm -f iccons
exit ${RETVAL}
