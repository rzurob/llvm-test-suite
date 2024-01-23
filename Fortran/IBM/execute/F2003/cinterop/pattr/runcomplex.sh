#!/usr/bin/ksh

xlcsmp="xlc"
echo ${OPTIONS} | grep -e '-qsmp' > /dev/null
if [[ "$?" -eq 0 ]]; then
    xlcsmp="xlc_r"
fi

if [ $(isAIX) != "0" ]; then  

    if test -f  /usr/include/complex.h
	then
	GCC="gcc"
	echo "${OPTIONS}" |grep q64 >/dev/null
	RC=$?
	if [ $RC = 0 ] 
	    then
	    XLCOPT="-q64"
	    GCCOPT="-m64"
	fi
	echo "Testing with gcc compiler..."
	echo 'Compiling C files'
	echo $GCC '-c' ${TR_SRC}/$2.c $GCCOPT -std=c99
	$GCC -c ${TR_SRC}/$2.c $GCCOPT -std=c99
	RC=$?
	

	if [ $RC = 0 ]
	    then
	    echo 'gcc Compile C file successful'
	else
	    echo 'gcc Compile C file failed'; exit 100
	fi
	
	CSUBOBJ=$2.o
	
	echo "Compiling Fortran files"
	echo "${COMPILER} ${TR_SRC}/assertmodule.f  ${TR_SRC}/$1.f $CSUBOBJ ${OPTIONS} -qfloat=complexgcc -o $1"
	${COMPILER} ${TR_SRC}/assertmodule.f ${TR_SRC}/$1.f $CSUBOBJ ${OPTIONS} -qfloat=complexgcc -o $1
	
	if [ $? -ne 0 ]; then
	    echo "Fortran code compile  error!!!..."
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
	rm -f $1.outs mod.mod $2.o a.out
	
	echo " Testing with xlc compiler"
	echo "Compiling C files"
	echo "xlc -c' ${TR_SRC}/$2.c $XLCOPT -qlanglvl=stdc99 -lm -qfloat=nocomplexgcc"
	xlc -c ${TR_SRC}/$2.c $XLCOPT -qlanglvl=stdc99 -lm -qfloat=nocomplexgcc
	RC=$?
	
	if [ $RC = 0 ]
	    then
	    echo 'xlc Compile C file successful'
	else
	    echo 'xlc Compile C file failed'; exit 130
	fi
	
	CSUBOBJ=$2.o
	
	echo 'Compiling Fortran files'
	echo "${COMPILER} ${TR_SRC}/assertmodule.f  ${TR_SRC}/$1.f $CSUBOBJ ${OPTIONS} -qfloat=nocomplexgcc -o $1"
	${COMPILER} ${TR_SRC}/assertmodule.f  ${TR_SRC}/$1.f $CSUBOBJ ${OPTIONS} -qfloat=nocomplexgcc -o $1
	
	if [ $? -ne 0 ]; then
	    echo "Fortran code compile  error!!!..."
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
	
	exit 0
	
    else
	echo "Complex.h do not exist in this system,Exit running." 
	exit 0
    fi

else 

    if test -f  /usr/include/complex.h
	then

	echo "Testing with xlc compiler..."

	echo "${OPTIONS}" |grep q64 >/dev/null
	RC=$?
	if [ $RC = 0 ]
	    then
	    XLCOPT="-q64"
	else 
	    XLCOPT=""
	fi

	echo "Compiling C code..."
	echo "$xlcsmp ${TR_SRC}/$2.c -c  $XLCOPT -qlanglvl=stdc99 ${OPTIONS} -lm"
	$xlcsmp ${TR_SRC}/$2.c  -c $XLCOPT -qlanglvl=stdc99 ${OPTIONS} -lm
	if [ $? -ne 0 ]; then
	    echo "C code compilation error!!!..."
	    exit 200
	fi

	CSUBOBJ=$2.o
	

	echo "Compiling Fortran files"
	echo "${COMPILER} ${TR_SRC}/assertmodule.f ${TR_SRC}/$1.f $CSUBOBJ ${OPTIONS} -o $1"
	${COMPILER} ${TR_SRC}/assertmodule.f  ${TR_SRC}/$1.f $CSUBOBJ ${OPTIONS} -o $1
   	if [ $? -ne 0 ]; then
	    echo "Fortran code compile  error!!!..."
	    exit 102
	fi

	echo "Executing..."
	./$1
	rc=$?
	if [ "$rc" -ne 0 ]; then
	    echo "Execution error!!!..."
	    exit $rc
	fi

    else
	echo "Complex.h do not exist in this system,Exit running."
	exit 0  
    fi
fi
