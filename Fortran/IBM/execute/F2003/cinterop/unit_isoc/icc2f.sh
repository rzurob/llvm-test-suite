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
echo ${CC} ${OPT64} -c $LDBLOPT -qlanglvl=stdc99 -qfloat=nocomplexgcc ${TR_SRC}/icc2fc$1.c
${CC} ${OPT64} -c $LDBLOPT -qlanglvl=stdc99 -qfloat=nocomplexgcc ${TR_SRC}/icc2fc$1.c
RETVAL=$?
if [ ${RETVAL} -ne 0 ]
  then exit ${RETVAL}
fi
echo ${COMPILER} ${TR_SRC}/icc2ff$1.F icc2fc$1.o -o icc2f$1 ${OPTIONS}
${COMPILER} ${TR_SRC}/icc2ff$1.F icc2fc$1.o -o icc2f$1 ${OPTIONS}
RETVAL=$?
if [ ${RETVAL} -eq 0 ]
  then echo ./icc2f$1
  ./icc2f$1
  RETVAL=$?
fi
echo rm -f icc2f$1 icc2fc$1.o
rm -f icc2f$1 icc2fc$1.o
exit ${RETVAL}
