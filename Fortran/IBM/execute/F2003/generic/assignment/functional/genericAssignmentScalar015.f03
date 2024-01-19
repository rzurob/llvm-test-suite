!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : assignment: only child type has generic assignment type bound
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
      integer :: i = -999
   end type

   type, extends(base) :: child
      integer, allocatable :: j
      contains
         procedure, private :: cassgn
         generic :: assignment(=) => cassgn
   end type

   contains

      subroutine cassgn ( a, b )
         class(child), intent(out) :: a
         class(child), intent(in) :: b

         a%base = b%base

         if ( .not. allocated ( a%j ) ) allocate ( a%j, source = -999 )

        if ( allocated ( b%j ) ) then
            a%j = b%j
         else
            a%j = -999
         end if

         print *, 'cassgn'

      end subroutine

end module

program genericAssignmentScalar015
   use m

   type (base) :: b1, b2
   class(base) , allocatable :: b3
   type(child) :: c1

   b1 = base(10)
   b2 = base(20)

   allocate ( b3, source = base(30) )

   print *, 'start'
   c1 = child(10, 20)

   deallocate ( b3 )

   allocate ( b3, source = child(0,0))

   select type ( b3 )
      class is ( child )
         b3 = c1
   end select

end program
