! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Allocate Statement (Section 6.3.1)
!*                               xi) Allocate unlimited polymorphic array with zero size abstract type-spec
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

program alloc009
   use m

   class(*), allocatable :: u1(:)
   class(*), pointer, dimension(:,:) :: u2

   allocate ( base :: u1(0) )
   allocate ( child :: u2(0,0) )

end program
