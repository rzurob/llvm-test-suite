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
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : assignment: operands with non-poly scalar (with pointer, allocatable attribute)
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


program genericAssignmentScalar002
   use m

   type(base), pointer :: b1
   type(base), allocatable :: b2

   type(base), target :: b3

   allocate ( b1, source = base(10) )
   allocate ( b2, source = b1 )

   if ( ( b1%i /= 10 ) .or. ( b2%i /= 10 ) ) error stop 1_4

   b3%i = 999
   b1 => b3

   b1 = b2

   if ( ( b1%i /= 10 ) .or. ( b2%i /= 10 ) .or. ( b3%i /= 10 ) ) error stop 2_4

   b3 = base(100)
   b2 = b1

   if ( ( b1%i /= 100 ) .or. ( b2%i /= 100 ) .or. ( b3%i /= 100 ) ) error stop 3_4

end program
