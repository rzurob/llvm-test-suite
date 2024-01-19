! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Implicit statement
!*                                        Implicit polymorphic abstract type array with allocate statement and pointer assignment
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
      contains
         procedure(getif), pass, deferred :: getid
   end type

   type, extends(base) :: child(k2)
      integer, kind :: k2
      integer(k2) :: rid
      contains
         procedure, pass :: getid
   end type

   interface
      integer function getif(a)
         import base
         class(base(4)), intent(in) :: a
      end function
   end interface

contains
   integer function getid(a)
      class(child(4,4)), intent(in) :: a
      getid = a%rid
   end function
end module

program implicit008
   use m
   implicit class(base(4)) (B)

   allocatable :: b1, b2(:)
   pointer :: b3(:)
   target :: b2
   allocate(child(4,4):: b1,b2(5),b3(0))

   deallocate (b1, b2, b3)

   allocate(b1, source = child(4,4)(2,3) )
   allocate(b2(2), source = (/child(4,4)(2,3),child(4,4)(3,4)/) )
   b3 => b2

   if ((b1%id .ne. 2) .or. (b1%getid()) .ne. 3) error stop 1_4
   if ((b3(1)%id .ne. 2) .or. (b3(1)%getid()) .ne. 3) error stop 2_4
   if ((b2(1)%id .ne. 2) .or. (b2(1)%getid()) .ne. 3) error stop 3_4
   if ((b3(2)%id .ne. 3) .or. (b3(2)%getid()) .ne. 4) error stop 4_4
   if ((b2(2)%id .ne. 3) .or. (b2(2)%getid()) .ne. 4) error stop 5_4

end program
