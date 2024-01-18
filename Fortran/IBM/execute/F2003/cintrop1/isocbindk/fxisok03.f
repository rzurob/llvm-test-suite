!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisok03 cxisok02
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
!*  KEYWORD(S)                 : C_CHAR, C_SIGNED_CHAR
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_CHAR and C_SIGNED_CHAR
!*      - using C functions with interface to FORTRAN functions
!*      - function interfaces defined in module
!*      - passing scalar arguments by REFERENCE and by VALUE
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob03
   interface
      integer(C_SIGNED_CHAR) function fnt1(a,b)
         use ISO_C_BINDING
         character(C_CHAR) :: a
         integer(C_SIGNED_CHAR) :: b
      end function fnt1
      integer(C_SIGNED_CHAR) function fnt2(a,b)
         use ISO_C_BINDING
         character(C_CHAR), value :: a
         integer(C_SIGNED_CHAR), value :: b
      end function fnt2
      integer(C_SIGNED_CHAR) function fnt3(a,b)
         use ISO_C_BINDING
         character(C_CHAR), intent(in) :: a
         integer(C_SIGNED_CHAR), intent(in) :: b
      end function fnt3
      integer(C_SIGNED_CHAR) function fnt4(a,b)
         use ISO_C_BINDING
         character(C_CHAR), intent(in), value :: a
         integer(C_SIGNED_CHAR), intent(in), value :: b
      end function fnt4
      integer(C_SIGNED_CHAR) function fnt5(a,b)
         use ISO_C_BINDING
         character(C_CHAR), intent(in) :: a
         integer(C_SIGNED_CHAR), intent(in) :: b
      end function fnt5
      integer(C_SIGNED_CHAR) function fnt6(a,b)
         use ISO_C_BINDING
         character(C_CHAR), intent(in), value :: a
         integer(C_SIGNED_CHAR), intent(in), value :: b
      end function fnt6
   end interface
end module mxisob03

program fxisok03
   use ISO_C_BINDING
   use mxisob03

   character(C_CHAR) :: a
   integer(C_SIGNED_CHAR) :: b
   integer :: ret

!! Test 1

   a = 'A'
   b = iachar('B')

   ret = fnt1(a,b)

   if ( a /= 'C' ) error stop 20
   if ( b /= iachar('D') ) error stop 22

!! Test 2

   a = 'A'
   b = iachar('B')

   ret = fnt2(a,b)

   if ( a /= 'A' ) error stop 24
   if ( b /= iachar('B') ) error stop 26

!! Test 3

   a = 'A'
   b = iachar('B')

   ret = fnt3(a,b)

   if ( a /= 'A' ) error stop 28
   if ( b /= iachar('B') ) error stop 30

!! Test 4

   a = 'A'
   b = iachar('B')

   ret = fnt4(a,b)

   if ( a /= 'A' ) error stop 32
   if ( b /= iachar('B') ) error stop 34

!! Test 5

   a = 'A'
   b = iachar('B')

   ret = fnt5(a,b)

   if ( a /= 'A' ) error stop 36
   if ( b /= iachar('B') ) error stop 38

!! Test 6

   a = 'A'
   b = iachar('B')

   ret = fnt6(a,b)

   if ( a /= 'A' ) error stop 40
   if ( b /= iachar('B') ) error stop 42

end program fxisok03
