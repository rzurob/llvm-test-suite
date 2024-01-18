! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/C503/localVar003.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: non-polymorphic abstract type entities in functions and subroutines (scalar, array, pointer, allocatable)
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
   type, abstract:: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i = 5
   end type

   type, extends(base) :: child(k2,n1)    ! (4,4,20)
       integer, kind :: k2
       integer, len  :: n1
   end type

contains

   integer function foo ()
      type(base(4)) :: b1
      type(base(4)), dimension(5) :: b2
      type(base(4)), pointer :: b3
      type(base(4)), allocatable, dimension(:) :: b4
      foo = 5
   end function

end module

program localVar003
end program

subroutine foo ()
   use m, only: base, newbase=>base
   type(newbase(4)) :: b1
   type(newbase(4)), dimension(5) :: b2
   call innerfoo()
contains
   subroutine innerfoo()
      type(newbase(4)), pointer :: b3
      type(newbase(4)), allocatable, dimension(:) :: b4
   end subroutine
end subroutine
