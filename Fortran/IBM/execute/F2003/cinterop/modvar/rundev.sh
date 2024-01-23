#!/usr/bin/ksh

function fortran {

  echo "Compiling Fortran code..."
  echo "${COMPILER} ${TR_SRC}/$1.f -c ${OPTIONS} $PDF"
  ${COMPILER} ${TR_SRC}/$1.f -c ${OPTIONS} $PDF
  if [ $? -ne 0 ]; then
    echo "Fortran code compilation error!!!..."
    exit 101
  fi

  echo "Linking object files..."
  echo "${COMPILER} $1.o $2.o $3.o $4.o  ${OPTIONS} $PDF -o $1"
  ${COMPILER} $1.o $2.o  $3.o $4.o ${OPTIONS} $PDF  -o $1
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
    rm -f $1 $1.o $2.o $3.o $4.o   *.mod
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


  echo "$xlcsmp ${TR_SRC}/$2.c ${TR_SRC}/$3.c ${TR_SRC}/$4.c  -c ${OPTIONS} $PDF"
  $xlcsmp  ${TR_SRC}/$2.c ${TR_SRC}/$3.c ${TR_SRC}/$4.c  $includefile  -c ${OPTIONS} $PDF

  if [ $? -ne 0 ]; then
    echo "C code compilation error!!!..."
    exit 201
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
  echo "gcc   ${TR_SRC}/$2.c    -c ${OPTIONS} $q64"
  $GCC ${TR_SRC}/$2.c $includefile -c ${OPTIONS} $q64
  if [ $? -ne 0 ]; then
    echo "C code compilation error!!!..."
    exit 300
  fi

  echo "gcc   ${TR_SRC}/$3.c    -c ${OPTIONS} $q64"
  $GCC ${TR_SRC}/$3.c $includefile -c ${OPTIONS} $q64
  if [ $? -ne 0 ]; then
    echo "C code compilation error!!!..."
    exit 301
  fi

  echo "gcc   ${TR_SRC}/$3.c    -c ${OPTIONS}  $q64 "
  $GCC ${TR_SRC}/$4.c -c $includefile ${OPTIONS} $q64
  if [ $? -ne 0 ]; then
    echo "C code compilation error!!!..."
    exit 302
  fi
  fortran $1 $2 $3 $4

fi
