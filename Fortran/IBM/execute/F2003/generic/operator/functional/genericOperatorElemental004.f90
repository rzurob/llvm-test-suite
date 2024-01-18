!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Operator: with elemental function with binary operator
!*                                         user defined operator defined in right operand
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


module m

   type :: base
      integer :: x = -999
      contains
         procedure :: mul
   end type

   type, extends(base) :: c1
      contains
         generic :: operator(*) => mul
   end type

   type, extends(base) :: c2
   end type

   contains

      type(base) elemental function mul (a, b)
         class(base), intent(in) :: a
         class(base), intent(in) :: b

         mul%x = a%x * b%x

      end function

end module

program genericOperatorElemental004
   use m

   type(base) :: ans(3)

   class(base), allocatable :: b1(:)

   type(c1) :: c11(3) = (/ c1(5), c1(6), c1(7) /)
   type(c2) :: c21(3) = (/ c2(15), c2(16), c2(17) /)

   allocate (  b1(3), source = c11 * (/ c2(10), c2(11),c2(12) /)  )

   print *, b1%x

   ans = c21 * c11
   print *, ans

   ans = c11 * c21
   print *, ans

   ans = c21 * c11 * c11
   print *, ans

   ans = b1 * c11
   print *, ans

end program
