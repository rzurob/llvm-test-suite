!***********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 4/23/2002
!*
!*  PRIMARY FUNCTIONS TESTED   : ISO_C_BINDING module
!*  SECONDARY FUNCTIONS TESTED : see below
!*
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
!*      - passing scalar arguments by REFERENCE and by VALUE
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisoi04
   use ISO_C_BINDING

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
      subroutine sub5(a)
         use ISO_C_BINDING
         integer(C_INT_FAST16_T), intent(in) :: a
      end subroutine sub5
      subroutine sub6(a)
         use ISO_C_BINDING
         integer(C_INT_FAST16_T), intent(in), value :: a
      end subroutine sub6
   end interface

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

!! Test 5

   a = 5

   call sub5(a)

   if ( a /= 5 ) error stop 28

!! Test 6

   a = 5

   call sub6(a)

   if ( a /= 5 ) error stop 30

end program fxisoi04