! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: non-polymorphic abstract type entities in modules (scalar, array, pointer, allocatable)
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
   use m, base => base

   type(base) :: b1
   type(base), dimension(5) :: b2
   type(base), pointer :: b3
   type(base), allocatable, dimension(:) :: b4

end module

program localVar002
end program

