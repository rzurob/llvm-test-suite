#!/bin/sh
CC=xlc
echo ${OPTIONS} | grep \\-q64
if [ $? -eq 0 ]
  then OPT64=\-q64
fi
echo ${CC} ${OPT64} -c -qlanglvl=stdc99 ${TR_SRC}/iconlyc$1.c
${CC} ${OPT64} -c -qlanglvl=stdc99 ${TR_SRC}/iconlyc$1.c
RETVAL=$?
if [ ${RETVAL} -ne 0 ]
  then exit ${RETVAL}
fi
echo ${COMPILER} ${TR_SRC}/iconlyf$1.f iconlyc$1.o -o iconly$1 ${OPTIONS}
${COMPILER} ${TR_SRC}/iconlyf$1.f iconlyc$1.o -o iconly$1 ${OPTIONS}
RETVAL=$?
if [ ${RETVAL} -eq 0 ]
  then echo ./iconly$1
  ./iconly$1
  RETVAL=$?
fi
echo rm -f iconly$1 iconlyc$1.o
rm -f iconly$1 iconlyc$1.o
exit ${RETVAL}
