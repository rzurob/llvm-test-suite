! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Select Type Construct with array
!*                               TYPE is (abstract type)
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

module m
   type, abstract :: base
      integer :: i = 5
   end type

   type, extends(base) :: child
   end type

end module

program selectType008
   use m

   class(base), allocatable :: b1(:)
   allocate (b1(2), source = (/ child(), child() /))
   select type ( b => b1 )
      type is (base)
         print *, 'error'
   end select

end program