!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisop00.presh fxisop03 cxisop02
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
!*  KEYWORD(S)                 : 16
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing 16
!*      - using C functions with interface to FORTRAN functions
!*      - function interfaces defined in module
!*      - passing scalar arguments by REFERENCE and by VALUE
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob03
   interface
      real(16) function fnt1(a)
         use ISO_C_BINDING
         real(16) :: a
      end function fnt1
      real(16) function fnt2(a)
         use ISO_C_BINDING
         real(16), value :: a
      end function fnt2
      real(16) function fnt3(a)
         use ISO_C_BINDING
         real(16), intent(in) :: a
      end function fnt3
      real(16) function fnt4(a)
         use ISO_C_BINDING
         real(16), intent(in), value :: a
      end function fnt4
      real(16) function fnt5(a)
         use ISO_C_BINDING
         real(16), intent(in) :: a
      end function fnt5
      real(16) function fnt6(a)
         use ISO_C_BINDING
         real(16), intent(in), value :: a
      end function fnt6
   end interface
end module mxisob03

program fxisop03
   use ISO_C_BINDING
   use mxisob03

   real(16) :: a, ret

!! Test 1

   a = 5.0q0

   ret = fnt1(a)

   if ( a /= 10.0q0 ) error stop 20

!! Test 2

   a = 5.0q0

   ret = fnt2(a)

   if ( a /= 5.0q0 ) error stop 22

!! Test 3

   a = 5.0q0

   ret = fnt3(a)

   if ( a /= 5.0q0 ) error stop 24

!! Test 4

   a = 5.0q0

   ret = fnt4(a)

   if ( a /= 5.0q0 ) error stop 26

!! Test 5

   a = 5.0q0

   ret = fnt5(a)

   if ( a /= 5.0q0 ) error stop 28

!! Test 6

   a = 5.0q0

   ret = fnt6(a)

   if ( a /= 5.0q0 ) error stop 30

end program fxisop03
