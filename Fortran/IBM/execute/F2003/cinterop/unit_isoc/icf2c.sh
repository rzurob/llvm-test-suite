#!/bin/sh
CC=xlc
if [ $(isAIX) = 0 ]
  then
  LDBLOPT=-qlongdouble
else
  LDBLOPT=-qdebug=aixldbl128
fi
echo ${OPTIONS} | grep \\-q64
if [ $? -eq 0 ]
  then OPT64=\-q64
fi
echo ${CC} ${OPT64} -c $LDBLOPT -qlanglvl=stdc99 -qfloat=nocomplexgcc ${TR_SRC}/icf2cc$1.c
${CC} ${OPT64} -c $LDBLOPT -qlanglvl=stdc99 -qfloat=nocomplexgcc ${TR_SRC}/icf2cc$1.c
RETVAL=$?
if [ ${RETVAL} -ne 0 ]
  then exit ${RETVAL}
fi
echo ${COMPILER} ${TR_SRC}/icf2cf$1.F icf2cc$1.o -o icf2c$1 ${OPTIONS}
${COMPILER} ${TR_SRC}/icf2cf$1.F icf2cc$1.o -o icf2c$1 ${OPTIONS}
RETVAL=$?
if [ ${RETVAL} -eq 0 ]
  then echo ./icf2c$1
  ./icf2c$1
  RETVAL=$?
fi
echo rm -f icf2c$1 icf2cc$1.o
rm -f icf2c$1 icf2cc$1.o
exit ${RETVAL}
