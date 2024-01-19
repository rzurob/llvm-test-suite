!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: initialization expression is not the same as assignment (generic or intrinsic)
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

         a%i = b%i + 1
         print *, a%i, '=', b%i

      end subroutine

end module


program genericAssignmentInit001
   use m

   type(base) :: b1 = base(10)  !<- initialization is not the same as assignment
                                ! (user-defined assignment should not be used)
   print *, b1%i
   b1 = base(20)

   print *, b1%i

end program
