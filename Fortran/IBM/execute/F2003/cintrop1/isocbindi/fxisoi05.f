!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
!***********************************************************************
! %START
! %MAIN:
! %PRECMD: ${TR_SRC}/scrisoi00.presh fxisoi05 cxisoi04
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
!*      - using C functions with interface to FORTRAN subroutines
!*      - subroutines interfaces defined in module
!*      - passing scalar arguments by REFERENCE and by VALUE
!*      - main written in C
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module mxisob05
   interface
      subroutine sub1(a)
         use ISO_C_BINDING
         integer(C_INT_FAST16_T) :: a
      end subroutine sub1
      subroutine sub2(a)
         use ISO_C_BINDING
         integer(C_INT_FAST16_T), value :: a
      end subroutine sub2
      subroutine sub3(a)
         use ISO_C_BINDING
         integer(C_INT_FAST16_T), intent(in) :: a
      end subroutine sub3
      subroutine sub4(a)
         use ISO_C_BINDING
         integer(C_INT_FAST16_T), intent(in), value :: a
      end subroutine sub4
   end interface
end module mxisob05

program fxisoi05
   use ISO_C_BINDING
   use mxisob05

   integer(C_INT_FAST16_T) :: a, ret

!! Test 1

   a = 5

   call sub1(a)

   if ( a /= 10 ) error stop 20

!! Test 2

   a = 5

   call sub2(a)

   if ( a /= 5 ) error stop 22

!! Test 3

   a = 5

   call sub3(a)

   if ( a /= 5 ) error stop 24

!! Test 4

   a = 5

   call sub4(a)

   if ( a /= 5 ) error stop 26

end program fxisoi05
