! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/dummyArg/dummy023.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  C503 The TYPE(derived-type-spec) shall not specify an abstract type
!*                                         c-a) OPTIONAL attribute with polymorphic abstract type (pointer or allocatable) with array
!*                                            1) if actual argument is associated, try
!*                                               i) polymorphic abstract type actual argument
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

module m

   type, abstract :: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: id
   end type

   type, extends(base) :: child(k2)    ! (4,4)
      integer, kind :: k2
      real(k2)      :: rid
   end type

contains

   subroutine foo(a, b)
      class(base(4)) :: a(:)
      class(base(4)), optional, allocatable :: b(:)

      if ((a(1)%id+a(2)%id) .ne. 9)    error stop 1_4
      if (present(b) ) then
         if ((b(1)%id+b(2)%id) .ne. 9) error stop 2_4
      end if
   end subroutine

   integer function boo(a, b)
      class(base(4)) :: a(2)
      class(base(4)), optional, pointer :: b(:)
      if ( present(b) ) then
         boo = a(1)%id + a(2)%id + b(1)%id + b(2)%id
      else
         boo = a(1)%id + a(2)%id
      end if
   end function

end module

program dummy023
   use m

   class(base(4)),  allocatable, target :: b1(:)
   class(base(4)),  pointer             :: b2(:)
   type(child(4,4)),  allocatable, target :: c1(:)

   allocate (b1(2), source = (/ child(4,4)(3,4.5), child(4,4)(6,7.8) /))
   allocate (c1(2), source = (/ child(4,4)(3,4.5), child(4,4)(6,7.8) /))

   b2 => b1

   call foo(b1, b1)
   call foo(c1, b1)

   if ( boo(c1, b2) .ne. 18 ) error stop 3_4
   if ( boo(b1, b2) .ne. 18 ) error stop 4_4

end program
