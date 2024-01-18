!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment(=)
!*
!*  DESCRIPTION                : Derived Type Intrinsic Assignment:
!*                                      - try a derived type containing an derived type scalar and array
!*                                        with elemental UD assignment in generic tb with class hierarchy
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
         procedure :: bassgn
         generic :: assignment(=) => bassgn
   end type

   type, extends(base) :: child
      integer :: j
   end type

   type container
      type(base) :: b1
      type(child) :: c1
      character(3) :: ccc
      type(base) :: b2(3)
      type(child) :: c2(2:4)
   end type

   contains

      elemental subroutine bassgn ( a, b )
         class(base), intent(out) :: a
         class(base), intent(in) :: b

         a%i = b%i + 1
         select type ( a )
            type is ( child )
               select type ( b )
                  type is ( child )
                     a%j = b%j + 1
               end select
         end select

      end subroutine

end module

program genericAssignmentDtIntrinAssgn012
   use m

   type(container) :: c1, c2
   type(container) :: c3

   pointer :: c2
   allocatable :: c3

   c1 = container(base(1), child(2,3), 'abc', (/ base(4), base(5), base(6) /), (/ child(7,8), child(9,10), child(11,12) /) )

   allocate ( c2, c3 )

   c2 = c1
   c3 = c2

   print *, c1
   print *, c2
   print *, c3

end program
