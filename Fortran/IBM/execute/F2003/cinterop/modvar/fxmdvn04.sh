#!/bin/ksh
echo "${OPTIONS}" |grep qsmp >/dev/null
     RC=$?
     if [ $RC = 0 ]
     then
      XLC="xlc_r -qsmp"
     else
      XLC="xlc"
     fi
     
if [ $(isAIX) != "0" ]; then
     GCC="gcc"
     echo "${OPTIONS}" |grep q64 >/dev/null
     RC=$?
     if [ $RC = 0 ]
     then
      XLCOPT="-q64"
      GCCOPT="-m64"
     fi
  
     if [ `isGCC_LD128` -eq 0 ]; then

     echo 'Compiling C files'
     echo $GCC '-c' ${TR_SRC}/$2.c -mlong-double-128 $GCCOPT -std=c99
     $GCC -c ${TR_SRC}/$2.c -mlong-double-128 $GCCOPT -std=c99
     RC=$?
  

     if [ $RC = 0 ]
     then
       echo 'gcc Compile C file successful'
     else
       echo 'gcc Compile C file failed'; exit 100
     fi
  
     CSUBOBJ=$2.o
   
     echo 'Compiling Fortran files'
     echo ${COMPILER} ${TR_SRC}/$1.f $CSUBOBJ ${OPTIONS}
     ${COMPILER} ${TR_SRC}/$1.f $CSUBOBJ ${OPTIONS}
     RC=$?

     if [ $RC = 0 ]
     then
      echo 'Compile file successful'
     else
      echo 'Compile file failed'; exit 110
     fi

     ./a.out >$1.outs
     RC=$?

     if [ $RC = 0 ]
     then
      echo 'Execution successful'
     else
      echo 'Execution failed'; exit 120
     fi

     rm -f $1.outs mod.mod $2.o a.out

     fi
     
     echo 'Compiling C files'
     echo $XLC ' -c' ${TR_SRC}/$2.c -qdebug=aixldbl128 $XLCOPT -qlanglvl=stdc99 -lm
     $XLC -c ${TR_SRC}/$2.c -qdebug=aixldbl128 $XLCOPT -qlanglvl=stdc99 -lm
     RC=$?
  

     if [ $RC = 0 ]
     then
       echo 'xlc Compile C file successful'
     else
       echo 'xlc Compile C file failed'; exit 130
     fi
  
     CSUBOBJ=$2.o
   
     echo 'Compiling Fortran files'
     echo ${COMPILER} ${TR_SRC}/$1.f $CSUBOBJ ${OPTIONS}
     ${COMPILER} ${TR_SRC}/$1.f $CSUBOBJ ${OPTIONS}
     RC=$?

     if [ $RC = 0 ]
     then
      echo 'Compile file successful'
     else
      echo 'Compile file failed'; exit 140
     fi

     ./a.out >$1.outs
     RC=$?

     if [ $RC = 0 ]
     then
      echo 'Execution successful'
     else
      echo 'Execution failed'; exit $RC
     fi

     rm -f $1.outs mod.mod $2.o a.out
     
     exit 0
     
     
else #AIX
    #OSLEV=`oslevel`
    #if [ "$OSLEV" = '4.3.3.0' -o "$OSLEV" = '5.0.0.0' -o \
    #    "$OSLEV" = '5.1.0.0' ]; then
    #  echo "This testcase is not supported in this OS level (AIX $OSLEV)..."
    #  echo "Testcase exiting..."
    #  exit 0
    #fi
    echo "${OPTIONS}" |grep q64 >/dev/null
    RC=$?
    if [ $RC = 0 ]
    then
      XLCOPT="-q64"
    else 
      XLCOPT=""
    fi
 
    echo 'Compiling C files'
    echo $XLC ' -c' ${TR_SRC}/$2.c -qlongdouble $XLCOPT -qlanglvl=stdc99 -lm
    $XLC -c ${TR_SRC}/$2.c -qlongdouble $XLCOPT -qlanglvl=stdc99 -lm
    RC=$?

    if [ $RC = 0 ]
    then
      echo 'xlc Compile C file successful'
    else
      echo 'xlc Compile C file failed'; exit 130
    fi

    CSUBOBJ=$2.o
  

    echo 'Compiling Fortran files'
    echo ${COMPILER} ${TR_SRC}/$1.f $CSUBOBJ ${OPTIONS}
    ${COMPILER} ${TR_SRC}/$1.f $CSUBOBJ ${OPTIONS}
    RC=$?

    if [ $RC = 0 ]
    then
      echo 'Compile file successful'
    else
      echo 'Compile file failed'; exit 200
    fi

    ./a.out >$1.outs
    RC=$?

    if [ $RC = 0 ]
    then
      echo 'Execution successful'
    else
      echo 'Execution failed'; exit $RC
    fi
    rm -f $1.outs mod.mod $2.o a.out

    exit 0
fi
