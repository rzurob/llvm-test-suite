!***********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: sign02.f
! %VERIFY: sign02.vf:sign02.out
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: diff sign02.out $TR_SRC/sign02.vf; rm-rf sign02.out
! %END
!***********************************************************************
!*
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Test of SIGN= specifier
!*      
!*  PROGRAMMER                 : James Ren
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : SIGN=specifier in WRITE
!*
!*  DRIVER STANZA              : xlf90/95
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : SIGN
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*  DESCRIPTION                : Testing the functionality of the
!*                               SIGN= specifier used in WRITE statement
!*                             1 The PROCESSOR_DEFINED is the default
!*                               SIGN mode, which is the same as SUPPRESS
!*                             2 The sign= specifier in WRITE statement
!*                               will temporarily change the SIGN mode
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890
implicit none
real r      
integer m
r = 10.5      
m = 20
write(*, 10) m, r
write(*, 10, sign = 'plus') m, r
write(*, 10, sign = 'processor_defined') m, r
write(*, 10, sign = 'suppress') m, r
10 format (I3, " ", F5.1)
end
