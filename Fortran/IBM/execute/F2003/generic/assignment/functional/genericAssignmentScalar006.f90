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
!*  DESCRIPTION                : assignment: operands with non-poly scalar with pointer dummy arg
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

      subroutine bassgnAlloc ( a, b )
         class(base), intent(out) :: a
         type(base), pointer, intent(in) :: b

         if ( associated ( b ) ) then
             a = b
         else
            a%i = -999
            print *, 'operand on RHS of assignment is disassociated'
         end if
      end subroutine

      subroutine bassgn ( a, b )
         class(base), intent(out) :: a
         type(base), intent(in) :: b

         a%i = b%i
         print *, a%i, '=', b%i

      end subroutine

end module


program genericAssignmentScalar006
   use m

   type(base) :: b1
   type(base), pointer :: b2
   type(base), allocatable :: b3

   b1 = base(10)
   allocate ( b2, b3 )

   b2 = base(20)
   b3 = base(30)

   b2 = b1
   if ( b2%i /= 10 ) error stop 1_4

   b1 = b3
   if ( b1%i /= 30 ) error stop 2_4

!   b1 = b2
   call bassgnAlloc (b1, b2)
   if ( b1%i /= 10 ) error stop 3_4

   nullify ( b2 )

!   b3 = b2
   call bassgnAlloc (b3, b2)
   if ( b3%i /= -999 ) error stop 4_4

end program
