#!/bin/sh
echo "${COMPILER} -c -qdebug=intmsg ${OPTIONS} ${TR_SRC}/bddiag01h.F 2> bddiag01h.err"
${COMPILER} -c -qdebug=intmsg ${OPTIONS} ${TR_SRC}/bddiag01h.F 2> bddiag01h.err
OS=`isAIX`
if [ $OS == 2 ]
then
  diff bddiag01h.err ${TR_SRC}/bddiag01h.Fvf.macos
  VAL=$?
else
  diff bddiag01h.err ${TR_SRC}/bddiag01h.Fvf
  VAL=$?
fi
rm -f bddiag01h.err
exit $VAL
