#!/bin/sh
CC=xlc
echo ${OPTIONS} | grep \\-q64
if [ $? -eq 0 ]
  then OPT64=\-q64
fi
if [ $# -eq 2 ]
  then CALIGN=\-qalign\=$2
       FALIGN=\-qalign\=bindc\=$2
fi
echo ${CC} ${OPT64} -c -qlanglvl=stdc99 -qextchk ${CALIGN} ${TR_SRC}/c$1.c
${CC} ${OPT64} -c -qlanglvl=stdc99 -qextchk ${CALIGN} ${TR_SRC}/c$1.c
RETVAL=$?
if [ ${RETVAL} -ne 0 ]
  then exit ${RETVAL}
fi

suse10up=0
if [ -r  /etc/SuSE-release ]
	then 
		echo   "grep -i 'version' /etc/SuSE-release | sed 's:.*=[       ]*::' "
		count=`grep -i 'version' /etc/SuSE-release | sed 's:.*=[       ]*::'`
		rc=`expr $count \>= 10`
		if  [ "$rc" = "1" ]
			then 
				echo "SUSE LINUX Enterprise Server Version is 10 or above 10"
				suse10up=1
			else 
				echo "SUSE LINUX Enterprise Server Version is 9 or under 9"
		fi
fi

if [ $suse10up -eq 0 ]
	then
		echo ${COMPILER} ${FALIGN} ${TR_SRC}/$1.F c$1.o -o $1 ${OPTIONS}
		${COMPILER} ${FALIGN} ${TR_SRC}/$1.F c$1.o -o $1 ${OPTIONS}
	else
		echo ${COMPILER} -WF,-DSUSE10_UP  ${FALIGN} ${TR_SRC}/$1.F c$1.o -o $1 ${OPTIONS}
		${COMPILER}  -WF,-DSUSE10_UP  ${FALIGN} ${TR_SRC}/$1.F c$1.o -o $1 ${OPTIONS}
fi

RETVAL=$?
if [ ${RETVAL} -eq 0 ]
  then echo ./$1
  ./$1
  RETVAL=$?
fi

echo rm -f $1 c$1.o
rm -f $1 c$1.o
exit ${RETVAL}
