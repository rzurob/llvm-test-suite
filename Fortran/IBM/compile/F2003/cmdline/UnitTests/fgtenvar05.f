! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qdebug=intmsg -qfixed
! %GROUP: redherring.f
! %VERIFY:
! %STDIN: 
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp fgtenvar05.f 
! %END
! *********************************************************************
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : For GET_ENVIRONMENT_VARIABLE intrinsic.
!*                             : 
!*  PROGRAMMER                 : Daniel Chen
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  DIAGNOSES TESTED           : If argument keywords are not specified,
!*                             : the order of the argument is critical.
!*
!*  DRIVER STANZA              : 
!*  REQUIRED COMPILER OPTIONS  : -qfixed
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program fgtenvar05
      
        character(10) :: ch1
        integer       :: i1, i2
        
        ! valid since arg kwd is spedified.
        call GET_ENVIRONMENT_VARIABLE(NAME="HOME", LENGTH=i1,
     +           STATUS=i2, VALUE=ch1)
        
        ! not valid.
        call GET_ENVIRONMENT_VARIABLE("HOME", i1, i2, ch1)
      
      End Program fgtenvar05
      
