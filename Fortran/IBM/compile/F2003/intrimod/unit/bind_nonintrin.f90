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
! %POSTCMD: dcomp bind_nonintrin.f; rm -f bind_nonintrin.o *.mod
! %END
! *********************************************************************
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : For module nature in USE statement.
!*                             :
!*  PROGRAMMER                 : Daniel Chen
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  DIAGNOSES TESTED           : The binding lable cannot be
!*                               the same as a nonintrinsic module.
!*
!*  DRIVER STANZA              :
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
       
       Module ieee_arithmetic
         integer bb
       end module

       subroutine ieee_exceptions() bind(c) 
         use, non_intrinsic ::  ieee_exceptions
       end subroutine ieee_exceptions

       subroutine sub1() bind(c, name="ieee_arithmetic") 
         use, non_intrinsic ::  ieee_arithmetic
       end subroutine sub1
