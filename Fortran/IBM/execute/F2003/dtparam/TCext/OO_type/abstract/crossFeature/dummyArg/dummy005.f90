! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/dummyArg/dummy005.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  C503 The TYPE(derived-type-spec) shall not specify an abstract type
!*                                         unlimited polymorphic dummy argument (non-pointer and non-allocatable) with
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

   type, abstract :: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: id
   end type

   type, extends(base) :: child(k2)    ! (4,4)
      integer, kind :: k2
      integer(k2)   :: rid
   end type

contains

   subroutine foo(a)
      class(*) :: a
      select type (a)
         class is (base(4))
            error stop 1_4
         class is (child(4,4))
            if (a%id .ne.  1)   error stop 2_4
            if (a%rid .ne. 2) error stop 3_4
      end select
   end subroutine

   integer function boo(a)
      class(*) :: a
      select type (a)
         class is (base(4))
            boo=1
         class is (child(4,4))
            boo=2
      end select
   end function

end module

program dummy005
   use m

   class(base(4)), allocatable :: b1
   type(child(4,4)), target :: c1 = child(4,4)(1,2)
   class(child(4,4)), pointer :: c2

   allocate (b1, source = child(4,4)(1,2))
   c2 => c1

   call foo(b1)
   call foo(c1)
   call foo(c2)

   if ( boo(b1) .ne. 2 ) error stop 4_4
   if ( boo(c1) .ne. 2 ) error stop 5_4
   if ( boo(c2) .ne. 2 ) error stop 6_4

end program
