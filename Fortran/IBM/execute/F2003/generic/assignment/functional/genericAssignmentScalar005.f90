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
!*  DESCRIPTION                : assignment: operands with non-poly scalar with allocatable dummy arg
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
      integer, allocatable :: i
      contains
         procedure, pass :: bassgn
         generic :: assignment(=) => bassgn
   end type

   contains

      subroutine bassgnAlloc ( a, b )
         class(base), intent(out) :: a
         type(base), allocatable, intent(in) :: b

         if ( .not. allocated ( a%i ) ) allocate ( a%i, source = -999 )

         if ( allocated ( b ) ) then
             a = b
         else
            print *, 'operand on right side of user-def assignment not allocated'
         end if

      end subroutine


      subroutine bassgn ( a, b )
         class(base), intent(out) :: a
         type(base), intent(in) :: b

         if ( .not. allocated ( a%i ) ) allocate ( a%i, source = -999 )

         a%i = b%i
         print *, a%i, '=', b%i
      end subroutine

end module


program genericAssignmentScalar005
   use m

   type(base) :: b1
   type(base), pointer :: b2
   type(base), allocatable :: b3

   b1 = base(10)
   allocate ( b2, source = base(20) )
   allocate ( b3, source = base( 100 ) )

   b1 = b3
   if ( b1%i /= 100  ) error stop 1_4

   b3%i = 200
   b2 = b3
   if ( b2%i /= 200  ) error stop 2_4

   b1 = b2
   if ( ( b1%i /= 200 ) .or. ( b2%i /= 200 )  ) error stop 3_4

   deallocate ( b3 )

   !b1 = b3
   call bassgnAlloc (b1, b3)

   if ( ( b1%i /= -999 ) .or. ( allocated ( b3 ) ) ) error stop 4_4

end program
