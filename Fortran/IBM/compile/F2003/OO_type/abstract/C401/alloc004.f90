! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Allocate Statement (Section 6.3.1)
!*                               iv) Allocate polymorphic non-abstract type scalar with abstract type-spec
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

   type:: base
      integer :: id
   end type

   type, extends(base), abstract :: child
      real :: rid
   end type

end module

program alloc004
   use m

   class(base), allocatable :: b1
   class(base), pointer     :: b2

   allocate ( child :: b1 )
   allocate ( child :: b2 )

end program
