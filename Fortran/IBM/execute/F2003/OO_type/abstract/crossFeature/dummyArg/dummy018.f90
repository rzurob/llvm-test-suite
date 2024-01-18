! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  C503 The TYPE(derived-type-spec) shall not specify an abstract type
!*                                         unlimited polymorphic dummy argument (non-pointer and non-allocatable) with array with
!*                                           a) polymorphic abstract type actual argument
!*                                           b) polymorphic extension type of abstract type actual argument
!*                                           c) non-polymorphic extension type of abstract type actual argument
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
      integer :: id
   end type

   type, extends(base) :: child
      integer :: rid
   end type

contains

   subroutine foo(a)
      class(*) :: a(:)

      select type (a)
         class is (base)
            error stop 1_4
         class is (child)
            if ((a(1)%id+a(2)%id) .ne. 9)          error stop 2_4
            if ((a(1)%rid+a(2)%rid) .ne. 11)     error stop 3_4
      end select

   end subroutine

   subroutine foo1(a)
      class(*) :: a(2,*)

      select type (a)
         class is (base)
            error stop 4_4
         class is (child)
            if ((a(1,1)%id+a(2,1)%id) .ne. 9)          error stop 5_4
            if ((a(1,1)%rid+a(2,1)%rid) .ne. 11)     error stop 6_4
      end select

   end subroutine

   integer function boo(a)
      class(*) :: a(2)
      select type (a)
         class is (base)
            error stop 7_4
         class is (child)
            boo=a(1)%id+a(2)%id
      end select
   end function

end module

program dummy018
   use m

   class(base), allocatable         :: b1(:)
   type(child), allocatable, target :: c1(:)
   class(child), pointer            :: c2(:)

   allocate (b1(2), source = (/ child(3,4), child(6,7) /))
   allocate (c1(2), source = (/ child(3,4), child(6,7) /))

   c2 => c1

   call foo(b1)
   call foo(c1)
   call foo(c2)

   call foo1(b1)
   call foo1(c1)
   call foo1(c2)

   if ( boo(b1) .ne. 9 ) error stop 8_4
   if ( boo(c1) .ne. 9 ) error stop 9_4
   if ( boo(c2) .ne. 9 ) error stop 10_4

end program