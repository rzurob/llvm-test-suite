! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/dummyArg/dummy007.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  C503 The TYPE(derived-type-spec) shall not specify an abstract type
!*                                         unlimited polymorphic dummy argument (pointer and allocatable) with
!*                                         d) unlimited actual argument
!*                                            1) of non-abstract dynamic type
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
      class(*), pointer :: a
      select type (a)
         class is (base(4))
            error stop 1_4
         class is (child(4,4))
            if (a%id .ne. 4)     error stop 2_4
            if (a%rid .ne. 5)  error stop 3_4
      end select
   end subroutine

   integer function boo(a)
      class(*), allocatable :: a
      select type (a)
         class is (base(4))
            boo=1
         class is (child(4,4))
            boo=2
      end select
   end function

end module

program dummy007
   use m

   class(*), allocatable, target :: u1
   class(*), pointer :: u2

   class(base(4)), allocatable :: b1
   class(child(4,4)), allocatable :: c1

   allocate (b1, source = child(4,4)(4,5))
   allocate (c1, source = child(4,4)(4,5))

   allocate (u1, source = b1)
   allocate (u2, source = c1)

   call foo(u2)

   if ( boo(u1) .ne. 2 )    error stop 4_4

end program