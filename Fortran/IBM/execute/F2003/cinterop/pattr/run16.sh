#!/usr/bin/ksh

function fortran {

    echo "Compiling Fortran code..."
    echo "${COMPILER} ${TR_SRC}/$1.f -c ${OPTIONS}"

    assertfunction=assertmodule
    ${COMPILER} ${TR_SRC}/$assertfunction.f  -c ${OPTIONS}
    if [ $? -ne 0 ]; then
	echo "Assertmodule compilation error!!!..."
	exit 111
    fi

    ${COMPILER}  ${TR_SRC}/$1.f  -c ${OPTIONS}
    if [ $? -ne 0 ]; then
	echo "Fortran code compilation error!!!..."
	exit 101
    fi

    echo "Linking object files..."
    echo "${COMPILER} $assertfunction.o $1.o $2.o ${OPTIONS} -o $1"
    ${COMPILER} $assertfunction.o  $1.o $2.o   ${OPTIONS} -o $1
    if [ $? -ne 0 ]; then
	echo "Link error!!!..."
	exit 102
    fi

    echo "Executing..."
    ./$1
    rc=$?
    if [ "$rc" -ne 0 ]; then
	echo "Execution error!!!..."
	exit $rc
    fi

    if [ "$TRUN_SAVE" != 'yes' ]; then
	rm -f $1 $1.o $2.o $assertfunction.o  *.mod
    fi

}

xlcsmp="xlc"
echo ${OPTIONS} | grep -e '-qsmp' > /dev/null
if [[ "$?" -eq 0 ]]; then
    xlcsmp="xlc_r"
fi

if [ `isAIX` -eq 0 ]; then
    echo "Compiling C code..."
    echo "$xlcsmp ${TR_SRC}/$2.c -c ${OPTIONS}"
    $xlcsmp ${TR_SRC}/$2.c  -c -qlanglvl=stdc99 ${OPTIONS} -qlongdouble  -lC128
 
    if [ $? -ne 0 ]; then
	echo "C code compilation error!!!..."
	exit 100
    fi
    fortran $1 $2

else
    echo "Option -qlongdouble only supported for xlc on AIX"
    exit 0
fi
