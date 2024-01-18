!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisol05 cxisol04
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
!*  DATE                       : 4/23/2002
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*
!*  PRIMARY FUNCTIONS TESTED   : ISO_C_BINDING module
!*  SECONDARY FUNCTIONS TESTED : see below
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : C_FLOAT, C_DOUBLE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_FLOAT and C_DOUBLE
!*      - using C functions with interface to FORTRAN subroutines
!*      - subroutines interfaces defined in module
!*      - passing scalar arguments by REFERENCE and by VALUE
!*      - main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob05
   interface
      subroutine sub1(a,b)
         use ISO_C_BINDING
         real(C_FLOAT) :: a
         real(C_DOUBLE) :: b
      end subroutine sub1
      subroutine sub2(a,b)
         use ISO_C_BINDING
         real(C_FLOAT), value :: a
         real(C_DOUBLE), value :: b
      end subroutine sub2
      subroutine sub3(a,b)
         use ISO_C_BINDING
         real(C_FLOAT), intent(in) :: a
         real(C_DOUBLE), intent(in) :: b
      end subroutine sub3
      subroutine sub4(a,b)
         use ISO_C_BINDING
         real(C_FLOAT), intent(in), value :: a
         real(C_DOUBLE), intent(in), value :: b
      end subroutine sub4
   end interface
end module mxisob05

program fxisol05
   use ISO_C_BINDING
   use mxisob05

   real(C_FLOAT) :: a, ret
   real(C_DOUBLE) :: b

!! Test 1

   a = 5.0e0
   b = 10.0d0

   call sub1(a,b)

   if ( a /= 10.0e0 ) error stop 20
   if ( b /= 20.0d0 ) error stop 22

!! Test 2

   a = 5.0e0
   b = 10.0d0

   call sub2(a,b)

   if ( a /= 5.0e0 ) error stop 24
   if ( b /= 10.0d0 ) error stop 26

!! Test 3

   a = 5.0e0
   b = 10.0d0

   call sub3(a,b)

   if ( a /= 5.0e0 ) error stop 28
   if ( b /= 10.0d0 ) error stop 30

!! Test 4

   a = 5.0e0
   b = 10.0d0

   call sub4(a,b)

   if ( a /= 5.0e0 ) error stop 32
   if ( b /= 10.0d0 ) error stop 34

end program fxisol05
