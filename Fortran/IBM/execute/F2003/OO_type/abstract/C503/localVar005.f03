! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: polymorphic abstract type entities in modules (scalar, array, pointer, allocatable)
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
   type , abstract:: base
      integer :: i = 5
   end type

   type, extends(base) :: child
   end type
end module

module m1
   use m
   class(base), pointer :: b3
   class(base), allocatable, dimension(:) :: b4

end module

program localVar005
   use m1, b1=>b3, b2=>b4

   allocate(b1, source=child(4) )
   allocate(child::b2(2))

   if (b1%i .ne. 4) error stop 1_4
   if (size(b2) .ne. 2) error stop 2_4

end program
