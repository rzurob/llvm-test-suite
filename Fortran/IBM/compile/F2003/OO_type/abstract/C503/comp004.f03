! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: C503 The TYPE(derived-type-spec) shall not specify an abstract type
!*					  Components are abstract type with default init, or private, public attributes
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
   end type

   type :: child1
      type(base), private, pointer :: bptr => null()
   end type

   type :: child2
      type(base), allocatable, public, dimension(:) :: balloc
   end type

end module

program comp004

end program