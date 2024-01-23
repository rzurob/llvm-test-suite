#!/usr/bin/ksh

function fortran {
  echo "Generate  object files..."
  echo "${COMPILER} ${TR_SRC}/$1.f $2.o $3.o $4.o  ${OPTIONS} $PDF -o $1"
  ${COMPILER}  ${TR_SRC}/$1.f $2.o $3.o $4.o ${OPTIONS} $PDF  -o $1
  if [ $? -ne 0 ]; then
    echo "code generation error!!!"
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
    rm -f $1 $1.o $2.o $3.o $4.o  *.mod
    if [ `echo ${OPTIONS} | grep -c -e '-qpdf2'` -ne 0 ]; then
      rm -f ._pdf_*
    fi
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
  $xlcsmp   ${TR_SRC}/$2.c   $includefile -c ${OPTIONS}
  if [ $? -ne 0 ]; then
    echo "C code compilation error!!!..."
    exit 100
  fi

  echo "$xlcsmp ${TR_SRC}/$3.c -c ${OPTIONS}"
  $xlcsmp   ${TR_SRC}/$3.c   $includefile -c ${OPTIONS}
  if [ $? -ne 0 ]; then
    echo "C code compilation error!!!..."
    exit 100
  fi

  echo "$xlcsmp ${TR_SRC}/$4.c -c ${OPTIONS}"
  $xlcsmp   ${TR_SRC}/$4.c   $includefile -c ${OPTIONS}
  if [ $? -ne 0 ]; then
    echo "C code compilation error!!!..."
    exit 100
  fi

  fortran $1 $2 $3 $4

else

  includefile="-Daix52"
  echo "Testing with $xlcsmp compiler..."
  echo "Compiling C code..."
  if [ `echo ${OPTIONS} | grep -c -e '-qpdf'` -ne 0 ]; then
    PDF="-qipa=pdfname=._pdf_xlc"
  fi

  echo "$xlcsmp ${TR_SRC}/$2.c ${TR_SRC}/$3.c -c  ${TR_SRC}/$4.c $includefile -qlanglvl=stdc99 ${OPTIONS} $PDF"
  $xlcsmp ${TR_SRC}/$2.c ${TR_SRC}/$3.c -c  ${TR_SRC}/$4.c  -c $includefile  -qlanglvl=stdc99 ${OPTIONS}  $PDF
  if [ $? -ne 0 ]; then
    echo "C code compilation error!!!..."
    exit 200
  fi
  fortran $1 $2 $3 $4
  
  GCC='gcc'
  echo ${OPTIONS} | grep -e '-q64' > /dev/null
  if [ "$?" -eq 0 ]; then
    q64='-m64'
  fi

  echo "Testing with gcc compiler..."
  echo "Compiling C code..."
  unset PDF
  echo "gcc   ${TR_SRC}/$2.c  ${TR_SRC}/$3.c  ${TR_SRC}/$4.c  $includefile -c ${OPTIONS} $q64"
  $GCC ${TR_SRC}/$2.c ${TR_SRC}/$3.c  ${TR_SRC}/$4.c  $includefile -c ${OPTIONS} $q64
  if [ $? -ne 0 ]; then
    echo "C code compilation error!!!..."
    exit 300
  fi
  fortran $1 $2 $3 $4

fi
