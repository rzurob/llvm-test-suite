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

   type:: base(k1)
      integer, kind :: k1
      integer :: id
   end type

   type, extends(base), abstract :: child(k2)
      integer, kind :: k2
      real(k2) :: rid
   end type

end module

program alloc004
   use m

   class(base(4)), allocatable :: b1
   class(base(4)), pointer     :: b2

   allocate ( child(4,4) :: b1 )
   allocate ( child(4,4) :: b2 )

end program
