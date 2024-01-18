#!/usr/bin/ksh

function fortran {
    cp ${TR_SRC}/$1.??? ./
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
	rm -f $1 $1.o $2.o $assertfunction.o  *.mod $1.???
    fi

}

xlcsmp="xlc"
echo ${OPTIONS} | grep -e '-qsmp' > /dev/null
if [[ "$?" -eq 0 ]]; then
    xlcsmp="xlc_r"
fi

if [ `isAIX` -eq 0 ]; then
    echo "Compiling C code..."
    echo "$xlcsmp ${TR_SRC}/$2.c -c -qlanglvl=stdc99 ${OPTIONS}"
    $xlcsmp ${TR_SRC}/$2.c  -c -qlanglvl=stdc99 ${OPTIONS}
    if [ $? -ne 0 ]; then
	echo "C code compilation error!!!..."
	exit 100
    fi
    fortran $1 $2

else
    echo "Testing with xlc compiler..."
    echo "Compiling C code..."
    echo "$xlcsmp ${TR_SRC}/$2.c -c -qlanglvl=stdc99 ${OPTIONS}"
    $xlcsmp ${TR_SRC}/$2.c  -c -qlanglvl=stdc99 ${OPTIONS}
    if [ $? -ne 0 ]; then
	echo "C code compilation error!!!..."
	exit 200
    fi
    fortran $1 $2

    GCC='gcc'
    echo ${OPTIONS} | grep -e '-q64' > /dev/null
    if [ "$?" -eq 0 ]; then
      q64='-m64'
    fi

    echo "Testing with gcc compiler..."
    echo "Compiling C code..."
    echo "gcc ${TR_SRC}/$2.c -c ${OPTIONS} $q64"
    $GCC ${TR_SRC}/$2.c $includefile -c ${OPTIONS} $q64
    if [ $? -ne 0 ]; then
	echo "C code compilation error!!!..."
	exit 300
    fi
    fortran $1 $2
fi
