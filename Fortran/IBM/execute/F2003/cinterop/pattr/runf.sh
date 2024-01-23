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
    ${COMPILER} $assertfunction.o  $1.o ${OPTIONS} -o $1
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
	rm -f $1 $1.o $assertfunction.o  *.mod
    fi

}

fortran $1
