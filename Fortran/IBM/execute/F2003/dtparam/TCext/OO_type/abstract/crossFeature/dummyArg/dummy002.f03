! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/dummyArg/dummy002.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing:  C503 The TYPE(derived-type-spec) shall not specify an abstract type
!*					   dummy argument defined to be abstract type.
!*                                         polymorphic abstract type dummy argument (non-pointer and non-allocatable) with
!*                                           a)polymorphic abstract type actual argument
!*                                           b)polymorphic extension type of abstract type actual argument
!*                                           c)non-polymorphic extension type of abstract type actual argument
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

   type, extends(base) :: child(k2,n1)    ! (4,4,20)
       integer, kind :: k2
       integer, len  :: n1
   end type

contains

   subroutine foo(a)
      class(base(4)) :: a
      if (a%id .ne. 3)   error stop 1_4
   end subroutine

   integer function boo(a)
      class(base(4)) :: a
      boo = a%id
   end function

end module

program dummy002
   use m

   class(base(4)), allocatable :: b1
   type(child(4,4,20)), target :: c1 = child(4,4,20)(3)
   class(child(4,4,20)), pointer :: c2

   allocate (b1, source = child(4,4,20)(3))
   c2 => c1

   call foo(b1)
   call foo(c1)
   call foo(c2)

   if ( boo(b1) .ne. 3 ) error stop 2_4
   if ( boo(c1) .ne. 3 ) error stop 3_4
   if ( boo(c2) .ne. 3 ) error stop 4_4

end program