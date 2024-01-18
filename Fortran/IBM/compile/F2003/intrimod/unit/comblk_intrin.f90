! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qdebug=intmsg -c
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp comblk_intrin.f; rm -f comblk_intrin.o *.mod
! %END
! *********************************************************************
!*
!*  ===================================================================
!*
!*                             :
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  DIAGNOSES TESTED           : The name of a common block can
!*                               be the same as an intrinsic module.
!*
!*  REQUIRED COMPILER OPTIONS  : -c
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

       Module ieee_exceptions
         integer aa
       end module

       use, intrinsic ::  ieee_exceptions
       common /ieee_exceptions/ i, j
       end
