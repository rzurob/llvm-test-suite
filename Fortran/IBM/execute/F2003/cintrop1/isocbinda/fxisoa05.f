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
!*  KEYWORD(S)                 : C_INT, C_SHORT
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*
!*      - testing C_INT and C_SHORT
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
         integer(C_INT) :: a
         integer(C_SHORT) :: b
      end subroutine sub1
      subroutine sub2(a,b)
         use ISO_C_BINDING
         integer(C_INT), value :: a
         integer(C_SHORT), value :: b
      end subroutine sub2
      subroutine sub3(a,b)
         use ISO_C_BINDING
         integer(C_INT), intent(in) :: a
         integer(C_SHORT), intent(in) :: b
      end subroutine sub3
      subroutine sub4(a,b)
         use ISO_C_BINDING
         integer(C_INT), intent(in), value :: a
         integer(C_SHORT), intent(in), value :: b
      end subroutine sub4
   end interface
end module mxisob05

program fxisoa05
   use ISO_C_BINDING
   use mxisob05

   integer(C_INT) :: a, ret
   integer(C_SHORT) :: b

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

end program fxisoa05
