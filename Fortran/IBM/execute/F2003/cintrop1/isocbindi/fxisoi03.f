!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoi00.presh fxisoi03 cxisoi02
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
!*  KEYWORD(S)                 : C_INT_FAST16_T
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_INT_FAST16_T
!*      - using C functions with interface to FORTRAN functions
!*      - function interfaces defined in module
!*      - passing scalar arguments by REFERENCE and by VALUE
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob03
   interface
      integer(C_INT_FAST16_T) function fnt1(a)
         use ISO_C_BINDING
         integer(C_INT_FAST16_T) :: a
      end function fnt1
      integer(C_INT_FAST16_T) function fnt2(a)
         use ISO_C_BINDING
         integer(C_INT_FAST16_T), value :: a
      end function fnt2
      integer(C_INT_FAST16_T) function fnt3(a)
         use ISO_C_BINDING
         integer(C_INT_FAST16_T), intent(in) :: a
      end function fnt3
      integer(C_INT_FAST16_T) function fnt4(a)
         use ISO_C_BINDING
         integer(C_INT_FAST16_T), intent(in), value :: a
      end function fnt4
      integer(C_INT_FAST16_T) function fnt5(a)
         use ISO_C_BINDING
         integer(C_INT_FAST16_T), intent(in) :: a
      end function fnt5
      integer(C_INT_FAST16_T) function fnt6(a)
         use ISO_C_BINDING
         integer(C_INT_FAST16_T), intent(in), value :: a
      end function fnt6
   end interface
end module mxisob03

program fxisoi03
   use ISO_C_BINDING
   use mxisob03

   integer(C_INT_FAST16_T) :: a, ret

!! Test 1

   a = 5

   ret = fnt1(a)

   if ( a /= 10 ) error stop 20

!! Test 2

   a = 5

   ret = fnt2(a)

   if ( a /= 5 ) error stop 22

!! Test 3

   a = 5

   ret = fnt3(a)

   if ( a /= 5 ) error stop 24

!! Test 4

   a = 5

   ret = fnt4(a)

   if ( a /= 5 ) error stop 26

!! Test 5

   a = 5

   ret = fnt5(a)

   if ( a /= 5 ) error stop 28

!! Test 6

   a = 5

   ret = fnt6(a)

   if ( a /= 5 ) error stop 30

end program fxisoi03
