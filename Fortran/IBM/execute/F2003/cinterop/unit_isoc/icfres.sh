#!/bin/sh
echo ${COMPILER} ${TR_SRC}/icfres$1.f -o icfres$1 ${OPTIONS}
${COMPILER} ${TR_SRC}/icfres$1.f -o icfres$1 ${OPTIONS}
RETVAL=$?
if [ ${RETVAL} -eq 0 ]
  then echo ./icfres$1
  ./icfres$1
  RETVAL=$?
fi
echo rm -f icfres$1 m.mod
rm -f icfres$1 m.mod
exit ${RETVAL}
