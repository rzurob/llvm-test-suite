!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/run.sh fxbind_c12xxs cxbind_c12xxs
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!***********************************************************************
!***********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : Support for ISO_C_BINDING module
!*
!*  PROGRAMMER                 : Alberto Alvarez-Mesquide
!*                               
!*  DATE                       : 4/23/2002
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ISO_C_BINDING module
!*  SECONDARY FUNCTIONS TESTED : see below 
!*
!*  DRIVER STANZA              : 
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 : C_INT
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                : 
!*	-  test allocatable actual arguments.
!*      -  test associated F90 pointers
!*      -  This testcase is modified base on Alerto's original 
!*         test case fxisopib00.f to add the bind(c) attribute.
!*        (Noted by Kantian, Mar 26,2004)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxbind_c12xxs
  use ISO_C_BINDING, ONLY : C_PTR, C_INT, C_LOC, C_ASSOCIATED, C_F_POINTER

  interface
     function sub1(x) bind(c)
       use ISO_C_BINDING, ONLY : C_PTR
       type(C_PTR) :: x
       int sub1
     end function sub1

  end interface
  integer:: result
  integer(C_INT), target :: a
  integer(C_INT), allocatable, target :: al
  integer(C_INT), pointer :: pa
  type(C_PTR) :: cp

  a = 5
  pa => a

  cp = C_LOC(a)
  if ( .not. C_ASSOCIATED(cp) ) error stop 20
  if ( .not. C_ASSOCIATED(cp,C_LOC(a)) ) error stop 22

  result = sub1(cp)
  if ( .not. C_ASSOCIATED(cp) ) error stop 24
  if ( C_ASSOCIATED(cp,C_LOC(a)) ) error stop 26

  call C_F_POINTER(cp,pa)
  if ( ASSOCIATED(pa,a) ) error stop 28
  if ( pa /= 10 ) error stop 30

  pa => a

  cp = C_LOC(pa)
  if ( .not. C_ASSOCIATED(cp) ) error stop 32
  if ( .not. C_ASSOCIATED(cp,C_LOC(pa)) ) error stop 34

  result =  sub1(cp)
  if ( .not. C_ASSOCIATED(cp) ) error stop 36
  if ( C_ASSOCIATED(cp,C_LOC(pa)) ) error stop 38

  call C_F_POINTER(cp,pa)
  if ( ASSOCIATED(pa,a) ) error stop 40
  if ( pa /= 10 ) error stop 42

end program fxbind_c12xxs
