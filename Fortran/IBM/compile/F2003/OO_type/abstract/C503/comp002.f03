! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: C503 The TYPE(derived-type-spec) shall not specify an abstract type
!*					  Components
!*                                        Components can be abstract type array(may or may not be zero size)
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
      type(base), dimension(5):: bb
   end type

   type :: child2
      type(base), dimension(0):: bb
   end type

end module

program comp002

end program