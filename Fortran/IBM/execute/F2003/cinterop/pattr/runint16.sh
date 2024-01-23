#!/usr/bin/ksh

function fortran {

    echo "Compiling Fortran code..."
    echo "${COMPILER} ${TR_SRC}/$1.f -c ${OPTIONS}"
    ${COMPILER} ${TR_SRC}/$1.f -c ${OPTIONS}
    if [ $? -ne 0 ]; then
	echo "Fortran code compilation error!!!..."
	exit 101
    fi

    echo "Linking object files..."
    echo "${COMPILER} $1.o $2.o ${OPTIONS} -o $1"
    ${COMPILER} $1.o $2.o ${OPTIONS} -o $1
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
	rm -f $1 $1.o $2.o  *.mod
    fi

}

xlcsmp="xlc"
echo ${OPTIONS} | grep -e '-qsmp' > /dev/null
if [[ "$?" -eq 0 ]]; then
    xlcsmp="xlc_r"

fi

if [ `isAIX` -eq 0 ]; then
    OSLEV=`oslevel`
    if [ "$OSLEV" = '4.3.3.0' -o "$OSLEV" = '5.0.0.0' -o \
        "$OSLEV" = '5.1.0.0' ]; then
	echo "This testcase is not supported in this OS level (AIX $OSLEV)..."
	echo "Testcase exiting..."
	exit 0
    fi

    includefile=""
    case $(oslevel) in
	5.2.*            ) includename="-Daix52" ;;
	*                ) ;;

    esac
    includefile="$includefile $includename"

    echo $includefile
    echo "Compiling C code..."
    echo "$xlcsmp ${TR_SRC}/$2.c -c ${OPTIONS}"
    $xlcsmp ${TR_SRC}/$2.c $includefile -c ${OPTIONS}

    if [ $? -ne 0 ]; then
	echo "C code compilation error!!!..."
	exit 100
    fi
    fortran $1 $2

else
    includefile="-Daix52" 
    echo "Testing with xlc compiler..."
    echo "Compiling C code..."
    echo "$xlcsmp ${TR_SRC}/$2.c -c ${OPTIONS}"
    $xlcsmp ${TR_SRC}/$2.c $includefile  -c ${OPTIONS}
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
