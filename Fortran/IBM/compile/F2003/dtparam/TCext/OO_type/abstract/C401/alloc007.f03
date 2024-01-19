! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Allocate Statement (Section 6.3.1)
!*                               vii) Allocate unlimited polymorphic scalar with abstract type-spec
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

   type, abstract :: base(k1)
      integer, kind :: k1
      integer(k1) :: id
   end type

   type, extends(base), abstract :: child(k2)
      integer, kind :: k2
      real(k2) :: rid
   end type

end module

program alloc007
   use m

   class(*), allocatable :: u1
   class(*), pointer     :: u2

   allocate ( base(4) :: u1 )
   allocate ( child(4,4) :: u2 )

end program
