!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoa00.presh fxisok05 cxisok04
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
         character(C_CHAR) :: a
         integer(C_SIGNED_CHAR) :: b
      end subroutine sub1
      subroutine sub2(a,b)
         use ISO_C_BINDING
         character(C_CHAR), value :: a
         integer(C_SIGNED_CHAR), value :: b
      end subroutine sub2
      subroutine sub3(a,b)
         use ISO_C_BINDING
         character(C_CHAR), intent(in) :: a
         integer(C_SIGNED_CHAR), intent(in) :: b
      end subroutine sub3
      subroutine sub4(a,b)
         use ISO_C_BINDING
         character(C_CHAR), intent(in), value :: a
         integer(C_SIGNED_CHAR), intent(in), value :: b
      end subroutine sub4
   end interface
end module mxisob05

program fxisok05
   use ISO_C_BINDING
   use mxisob05

   character(C_CHAR) :: a
   integer(C_SIGNED_CHAR) :: b
   integer :: ret

!! Test 1

   a = 'A'
   b = iachar('B')

   call sub1(a,b)

   if ( a /= 'C' ) error stop 20
   if ( b /= iachar('D') ) error stop 22

!! Test 2

   a = 'A'
   b = iachar('B')

   call sub2(a,b)

   if ( a /= 'A' ) error stop 24
   if ( b /= iachar('B') ) error stop 26

!! Test 3

   a = 'A'
   b = iachar('B')

   call sub3(a,b)

   if ( a /= 'A' ) error stop 28
   if ( b /= iachar('B') ) error stop 30

!! Test 4

   a = 'A'
   b = iachar('B')

   call sub4(a,b)

   if ( a /= 'A' ) error stop 32
   if ( b /= iachar('B') ) error stop 34

end program fxisok05
