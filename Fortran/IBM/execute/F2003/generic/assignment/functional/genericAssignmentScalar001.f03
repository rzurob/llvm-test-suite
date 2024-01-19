!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: operands with non-poly scalar
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

   type base
      integer :: i
      contains
         procedure, pass :: bassgn
         generic :: assignment(=) => bassgn
   end type


   contains

      subroutine bassgn ( a, b )
         class(base), intent(out) :: a
         type(base), intent(in) :: b

         a%i = b%i
         print *, a%i, '=', b%i

      end subroutine

end module


program genericAssignmentScalar001
   use m

   type(base) :: b1, b2


   b1 = base(10)
   b2 = base(20)

   if ( ( b1%i /= 10 ) .or. ( b2%i /= 20 ) ) error stop 1_4

   b1 = b2

   if ( ( b1%i /= 20 ) .or. ( b2%i /= 20 ) ) error stop 2_4


end program