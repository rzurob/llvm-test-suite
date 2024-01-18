! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qdebug=intmsg
! %GROUP: redherring.f
! %VERIFY:
! %STDIN: 
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp fgcmdarg01.f 
! %END
! *********************************************************************
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : For GET_COMMAND_ARGUMENT intrinsic.
!*                             : 
!*  PROGRAMMER                 : Daniel Chen
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  DIAGNOSES TESTED           : If one argument keyword is specified,
!*                             : the subsequent argument keywords must
!*                             : be specified as well.
!*
!*  DRIVER STANZA              : 
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : 
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      Program fgcmdarg01
      
        integer :: i1, i2, i3
        character(10) :: ch1
        
        i1=0
        call GET_COMMAND_ARGUMENT(NUMBER=i1, ch1, i2, i3)
      
      End Program fgcmdarg01