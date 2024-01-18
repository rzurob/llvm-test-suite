! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: C503 The TYPE(derived-type-spec) shall not specify an abstract type
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
end module

program typeDeclr001
   use m

   type(base) :: b0
   type(base), allocatable :: b1
   type(base), pointer, dimension(:,:) :: b2
   type(base), dimension(5) :: b3
   type(base), parameter :: b4 = base(4)

end program