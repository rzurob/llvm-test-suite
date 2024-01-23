#!/usr/bin/ksh

#----------------------------------------------------------------------------------------------
#-- This script will be used to compile programs that must not be run with -q64 option.
#----------------------------------------------------------------------------------------------
# ...let's strip -q64 then
OPTIONS=$(echo $OPTIONS|sed s/-q64//g)

OSTYPE=$(uname)
case $OSTYPE in
"AIX")
   OS_RC=0 ;;
"Linux")
   OS_RC=1 ;;
"Darwin")
   OS_RC=2 ;;
*)
   OS_RC=3 ;;
esac

#----------------------------------------------------------------------------------------------
#-- Unknown OS (program exits with 222 if OS is unknown; this is for further down
#-- the line when (and if) there are other OSes supported, this TC should flag it for a
#-- proper fix
#----------------------------------------------------------------------------------------------

if [[ $OS_RC -eq 3 ]]; then

	echo "   *** This is an unknown OS --- EXITING PROGRAM ***  "
        exit 222

fi

#----------------------------------------------------------------------------------------------
#-- Exit program if it is run on an AIX version lower than AIX5L 
#----------------------------------------------------------------------------------------------

if [[ $OS_RC -eq 0 ]]; then

   OSLEVEL=$(oslevel)
   isAIX5L=$(echo $OSLEVEL  | cut -f1  -d".")

   if [[ $isAIX5L -lt 5 ]]; then

        echo " ***  This program must not be run on AIX $OSLEVEL "
        echo " ***               Exiting program             *** "
        exit 222

   fi
fi


#----------------------------------------------------------------------------------------------
#-- Exit program if run with -q64 
#----------------------------------------------------------------------------------------------

echo ${OPTIONS} | grep -q "\-q64"
RC=$?
if [[ ${RC} -eq 0 ]]; then
   echo " ***  This program must not be run with -q64 option "
   echo " ***              Exiting program               *** "
   exit 222
fi



#----------------------------------------------------------------------------------------------
#-- Set up -qalign sub-options for AIX, Linux and Mac.
#----------------------------------------------------------------------------------------------

if [[ $OS_RC -eq 0 ]]; then
   set -A ALG 'bit_packed' 'full' 'mac68k' 'natural' 'packed' 'power' 'twobyte'
elif [[ $OS_RC -eq 1 ]]; then
   set -A ALG 'bit_packed' 'linuxppc'
elif [[ $OS_RC -eq 2 ]]; then
   set -A ALG 'natural' 'packed' 'port'
fi


#----------------------------------------------------------------------------------------------
#-- Function to compile Fortran code, and link it with C-code.
#----------------------------------------------------------------------------------------------

function compile_Fortran {

  echo Compiling Fortran file ...
  echo ${KILL} ${COMPILER} ${TR_SRC}/${1}.f -c  ${OPTIONS} ${align_F} ${PDF}
  ${KILL} ${COMPILER} ${TR_SRC}/${1}.f -c ${OPTIONS}  ${align_F} ${PDF}
  RC=$?

  if [[ ${RC} = 0 ]]; then
    echo Compilation successful for file ${1}.f.
  else
    echo " ***"   Compilation failed for file ${1}.f.; exit 201
  fi


  echo --------------------------------------------------
  echo

  echo Linking Fortran .o file with C .o file ...
  echo ${KILL} ${COMPILER} -o ./${1} ./${1}.o ./${2}.o ${OPTIONS} ${align_F} ${PDF}
  ${KILL} ${COMPILER} -o ./${1} ./${1}.o ./${2}.o ${OPTIONS} ${align_F} ${PDF}
  RC=$?

  if [[ ${RC} = 0 ]]; then
    echo Linking ./${1}.o ./${2}.o successful.
  else
    echo " ***"   Linking ./${1}.o ./${2}.o failed.; exit 202
  fi

  echo --------------------------------------------------
  echo

  echo "Executing ./${1} ..."
  echo "${KILL} ./${1}  >${1}.out"
  ${KILL} ./${1}  >${1}.out
  RC=$?

  if [[ ${RC} = 0 ]]; then
    echo Execution successful.
  else
    echo " ***"   Execution failed.; exit ${RC}
  fi


  echo --------------------------------------------------
  echo

  if [[ $TRUN_SAVE != "YES" && $TRUN_SAVE != "yes" ]]; then
    rm -f *.mod $1.o $2.o $1 $1.out 

    #-- Clean-up the ._pdf files after -qpdf2 runs complete.
    echo ${OPTIONS} | grep -q '\-qpdf2'
    RC=$?
    if [[ ${RC} -eq 0 ]]; then
        rm  -f ._pdf_${align} 
    fi


  fi

}

#----------------------------------------------------------------------------------------------
#-- Function to compile C code
#----------------------------------------------------------------------------------------------

#-- Remove invalid OPTIONS for C
OPTIONSc=`echo ${OPTIONS} | sed 's/\-qfree\=f90//g' |  sed 's/\-qfixed//g' | sed 's/\-qflag\=l\:l//g'`

function compile_C {
   echo Compiling C file using xlc ...
   echo ${KILL} xlc ${TR_SRC}/${2}.c -c -qnolm -qlanglvl=stdc99 ${OPTIONSc} ${align_C} ${DINT} ${PDF}
   ${KILL} xlc ${TR_SRC}/${2}.c  -c -qnolm -qlanglvl=stdc99 ${OPTIONSc} ${align_C} ${DINT} ${PDF}
   RC=$?
   if [[ ${RC} = 0 ]]; then
     echo Compilation successful for file ${2}.c
     echo
   else
     echo " ***"   Compilation failed for file ${2}.c.; exit 200
     echo
   fi
}


#----------------------------------------------------------------------------------------------
#-- Function to Compile, Link, Execute C and Fortran programs with different alignments
#----------------------------------------------------------------------------------------------

function RUN_PROG {

   #-- Compile with no -qalign option
   echo
   echo "Test with alignment:  NONE (default) ..."
   echo

   align_C=
   align_F=

   echo ${OPTIONS} | grep -q '\-qpdf'
   RC=$?
   if [[ ${RC} -eq 0 ]]; then
      PDF=-qipa=pdfname=._pdf_xlc
   fi

   compile_C $1 $2
   compile_Fortran $1 $2


   #-- Compile with -qalign options
   for align in ${ALG[*]}; do

      echo
      echo "Test with alignment:  $align  ..."
      echo

      align_C=-qalign=${align}
      align_F=-qalign=bindc=${align}

      echo ${OPTIONS} | grep -q '\-qpdf' 
      RC=$?
      if [[ ${RC} -eq 0 ]]; then
         PDF=-qipa=pdfname=._pdf_${align}
      fi

      compile_C $1 $2
      compile_Fortran $1 $2

   done

}


#----------------------------------------------------------------------------------------------
#-- AIX 
#----------------------------------------------------------------------------------------------

unset DINT
unset PDF

if [[ $OS_RC -eq 0 ]]; then

   if [[ $isAIX5L -ge 5 ]]; then

       isAIX52=$(echo $OSLEVEL | cut -f-2  -d"." | cut -f2  -d".")
       if [[ $isAIX52 -ge 2 ]]; then

          DINT='-DSTDINT'

       fi
   fi

   RUN_PROG $1 $2


#----------------------------------------------------------------------------------------------
#-- Linux and Mac (need to test with gcc as well)
#----------------------------------------------------------------------------------------------

else

   DINT='-DSTDINT'

   RUN_PROG $1 $2
 

   #-- Compile with gcc; with gcc no -qalign options can be tested.

   echo
   echo "GCC: Test with alignment: NONE ..."
   echo

   align_C=
   align_F=
   unset PDF
   OPTIONSgcc=`echo ${OPTIONSc} | sed 's/\-qpdf1/ /g' | sed 's/\-qpdf2/ /g'`

   echo Compiling C file using GCC ...
   echo ${KILL} gcc ${TR_SRC}/$2.c -c ${OPTIONSgcc} $DINT 
   ${KILL} gcc ${TR_SRC}/$2.c -c ${OPTIONSgcc} $DINT
   RC=$?
   if [[ ${RC} = 0 ]]; then
     echo Compilation successful for file ${2}.c
   else
     echo " ***"   Compilation failed for file ${2}.c.; exit 200
   fi

   compile_Fortran $1 $2

fi

