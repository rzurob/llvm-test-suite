! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: If the rightmost part-name is of abstract type, data-ref shall be polymorphic. (C611)
!*                                        Non-rightmost part-name a scalar object, with intrinsic assignment
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

   type, extends(base) :: child
      real :: rid
   end type

end module

program structComp002
   use m

   type(child) :: c1

   type(child), target :: c11
   type(child), target, allocatable :: c12
   class(child), pointer :: c13

   c1%base = c11%base

   allocate (c12, source = child(1,2.3) )
   c13 => c12

   c1%base = c12%base
   c1%base = c13%base

end program