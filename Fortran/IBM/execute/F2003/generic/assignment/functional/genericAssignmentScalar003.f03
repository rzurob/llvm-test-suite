!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: operands with non-poly scalar with pointer components
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
      integer, pointer :: i => null()
      contains
         procedure, pass :: bassgn
         generic :: assignment(=) => bassgn
   end type


   contains

      subroutine bassgn ( a, b )
         class(base), intent(out) :: a
         type(base), intent(in) :: b

         if ( .not. associated ( a%i ) ) allocate ( a%i )

         a%i => b%i
         print *, a%i, '=>', b%i

      end subroutine

end module


program genericAssignmentScalar003
   use m

   type(base), pointer :: b1
   type(base), allocatable :: b2
   type(base), target :: b3

   allocate ( b1, source = base() )
   allocate ( b1%i, source = 10 )

   allocate ( b2 )
   b2 = b1

   if ( ( b1%i /= 10 ) .or. ( b2%i /= 10 ) ) error stop 1_4

   b2%i = 100
   if ( ( b1%i /= 100 ) .or. ( b2%i /= 100 ) ) error stop 2_4

   b3 = b1
   if ( ( b1%i /= 100 ) .or. ( b2%i /= 100 ) .or. ( b3%i /= 100 ) ) error stop 3_4
   if ( ( .not. ( associated( b3%i, b2%i ) ) ) .or.  ( .not. ( associated( b3%i, b1%i ) ) ) ) error stop 4_4

   deallocate ( b1, b2 )

   allocate ( b1, b2 )
   allocate ( b1%i, b2%i )

   if ( associated ( b1%i, b2%i ) ) error stop 5_4

   b2%i = 1000
   b1 = b2

   if ( ( b1%i /= 1000 ) .or. ( b2%i /= 1000 ) .or. ( .not. associated ( b1%i , b2%i ) ) ) error stop 6_4

end program