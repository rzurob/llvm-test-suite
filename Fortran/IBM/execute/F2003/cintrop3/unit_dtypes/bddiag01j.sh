#!/bin/sh
${COMPILER} -qdebug=intmsg ${OPTIONS} ${TR_SRC}/bddiag01j.F 2> bddiag01j.err
OS=`isAIX`
if [ $OS == 2 ]
then
  diff bddiag01j.err ${TR_SRC}/bddiag01j.Fvf.macos
  VAL=$?
else
  diff bddiag01j.err ${TR_SRC}/bddiag01j.Fvf
  VAL=$?
fi
rm -f bddiag01j.err
exit $VAL
