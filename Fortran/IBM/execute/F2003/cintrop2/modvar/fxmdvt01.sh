#!/bin/ksh
echo "${OPTIONS}" |grep qsmp >/dev/null
     RC=$?
     if [ $RC = 0 ]
     then
      XLC="xlc_r -qsmp"
     else
      XLC="xlc"
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
   
     
if [ $(isAIX) != "0" ]; then
     echo "${OPTIONS}" |grep q64 >/dev/null
     RC=$?
     if [ $RC = 0 ]
     then
      XLCOPT="-q64"
     fi
  
     echo 'Compiling C files'
     echo $XLC ' -c' ${TR_SRC}/$2.c $XLCOPT '-qextchk'
     $XLC  -c ${TR_SRC}/$2.c $XLCOPT -qextchk
     RC=$?
  

     if [ $RC = 0 ]
     then
       echo 'xlc Compile C file successful'
     else
       echo 'xlc Compile C file failed'; exit 130
     fi
  
     CSUBOBJ=$2.o
   
    echo 'Compiling Fortran files'

    if [ -e  ${TR_SRC}/$1.F ]
    then
        if [ $suse10up -eq 0 ]
        then
                echo ${COMPILER} ${TR_SRC}/$1.F $CSUBOBJ ${OPTIONS}
                ${COMPILER} ${TR_SRC}/$1.F $CSUBOBJ ${OPTIONS}
        else
                echo ${COMPILER} -WF,-DSUSE10_UP  ${TR_SRC}/$1.F $CSUBOBJ ${OPTIONS}
                ${COMPILER} -WF,-DSUSE10_UP  ${TR_SRC}/$1.F $CSUBOBJ ${OPTIONS}
        fi
    else
          echo ${COMPILER} ${TR_SRC}/$1.f $CSUBOBJ ${OPTIONS}
          ${COMPILER} ${TR_SRC}/$1.f $CSUBOBJ ${OPTIONS}
    fi

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
     
     
else

    echo "${OPTIONS}" |grep q64 >/dev/null
    RC=$?
    if [ $RC = 0 ]
    then
      XLCOPT="-q64"
    else 
      XLCOPT=""
    fi
 
    echo 'Compiling C files'
    echo $XLC ' -c' ${TR_SRC}/$2.c $XLCOPT '-qextchk'
    $XLC -c ${TR_SRC}/$2.c $XLCOPT -qextchk
    RC=$?

    if [ $RC = 0 ]
    then
      echo 'xlc Compile C file successful'
    else
      echo 'xlc Compile C file failed'; exit 130
    fi

    CSUBOBJ=$2.o
  

    echo 'Compiling Fortran files'

    if [ -e  ${TR_SRC}/$1.F ]
    then
        if [ $suse10up -eq 0 ]
        then
                echo ${COMPILER} ${TR_SRC}/$1.F $CSUBOBJ ${OPTIONS}
                ${COMPILER} ${TR_SRC}/$1.F $CSUBOBJ ${OPTIONS}
        else
                echo ${COMPILER} -WF,-DSUSE10_UP  ${TR_SRC}/$1.F $CSUBOBJ ${OPTIONS}
                ${COMPILER} -WF,-DSUSE10_UP  ${TR_SRC}/$1.F $CSUBOBJ ${OPTIONS}
        fi
    else
          echo ${COMPILER} ${TR_SRC}/$1.f $CSUBOBJ ${OPTIONS}
          ${COMPILER} ${TR_SRC}/$1.f $CSUBOBJ ${OPTIONS}
    fi

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
                                                                                                                                                                                                                                                                                                                                                                                             