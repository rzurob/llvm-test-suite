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
!*  KEYWORD(S)                 : C_LONG, C_LONG_LONG
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_LONG and C_LONG_LONG
!*      - using C functions with interface to FORTRAN subroutines
!*      - passing scalar arguments by REFERENCE and by VALUE
!*      - main written in FORTRAN
!*
!234567890123456789012345678901234567890123456789012345678901234567890

program fxisob04
   use ISO_C_BINDING

   interface
      subroutine sub1(a,b)
         use ISO_C_BINDING
         integer(C_LONG) :: a
         integer(C_LONG_LONG) :: b
      end subroutine sub1
      subroutine sub2(a,b)
         use ISO_C_BINDING
         integer(C_LONG), value :: a
         integer(C_LONG_LONG), value :: b
      end subroutine sub2
      subroutine sub3(a,b)
         use ISO_C_BINDING
         integer(C_LONG), intent(in) :: a
         integer(C_LONG_LONG), intent(in) :: b
      end subroutine sub3
      subroutine sub4(a,b)
         use ISO_C_BINDING
         integer(C_LONG), intent(in), value :: a
         integer(C_LONG_LONG), intent(in), value :: b
      end subroutine sub4
      subroutine sub5(a,b)
         use ISO_C_BINDING
         integer(C_LONG), intent(in) :: a
         integer(C_LONG_LONG), intent(in) :: b
      end subroutine sub5
      subroutine sub6(a,b)
         use ISO_C_BINDING
         integer(C_LONG), intent(in), value :: a
         integer(C_LONG_LONG), intent(in), value :: b
      end subroutine sub6
   end interface

   integer(C_LONG) :: a, ret
   integer(C_LONG_LONG) :: b

!! Test 1

   a = 5
   b = 10

   call sub1(a,b)

   if ( a /= 10 ) error stop 20
   if ( b /= 20 ) error stop 22

!! Test 2

   a = 5
   b = 10

   call sub2(a,b)

   if ( a /= 5 ) error stop 24
   if ( b /= 10 ) error stop 26

!! Test 3

   a = 5
   b = 10

   call sub3(a,b)

   if ( a /= 5 ) error stop 28
   if ( b /= 10 ) error stop 30

!! Test 4

   a = 5
   b = 10

   call sub4(a,b)

   if ( a /= 5 ) error stop 32
   if ( b /= 10 ) error stop 34

!! Test 5

   a = 5
   b = 10

   call sub5(a,b)

   if ( a /= 5 ) error stop 36
   if ( b /= 10 ) error stop 38

!! Test 6

   a = 5
   b = 10

   call sub6(a,b)

   if ( a /= 5 ) error stop 40
   if ( b /= 10 ) error stop 42

end program fxisob04
