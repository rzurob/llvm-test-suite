!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Allocate Statement (Section 6.3.1)
!*                               x) Allocate scalar/array with abstract type-spec (with rename-list in use stmt)
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

   type, abstract :: base
      integer :: id
   end type

   type, extends(base), abstract :: child
      real :: rid
   end type

end module

program alloc010
   use m, abstractbase => base, abstractchild => child

   class(abstractbase), allocatable :: b1
   class(abstractchild), pointer    :: c1(:,:)

   allocate ( abstractbase :: b1 )
   allocate ( c1(2,2) )

end program
