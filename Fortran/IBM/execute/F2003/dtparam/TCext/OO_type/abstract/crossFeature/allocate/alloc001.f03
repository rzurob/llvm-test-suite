! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/allocate/alloc001.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Allocate statement - type-spec cannot be non-poly abstract type
!*                                        unlimited polymorphic entity to be allocated with unlimited
!*                                        polymorphic (that is extension type of abstract type) and polymorphic
!*                                        abstract type
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

   type, abstract :: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: id
   end type

   type, extends(base) :: child(k2)    ! (4,4)
      integer, kind :: k2
      integer(k2)   :: rid
   end type

end module

program alloc001
   use m

   class(*), pointer   :: u1
   class(base(4)), allocatable, dimension(:) :: u2
   class(*), allocatable :: u3

   class(base(4)), pointer :: b1

   allocate(b1, source = child(4,4)(2,3) )

   allocate( u3, source = b1 )
   allocate( u1, source = u3 )
   allocate( u2(2), source = (/b1,b1/) )

   select type (u1)
      class is (base(4))
         if (u1%id .ne. 2) error stop 1_4
      class default
         error stop 2_4
   end select

   select type (u2)
      class is (child(4,4))
         if (u2(1)%id  .ne. 2) error stop 3_4
         if (u2(2)%id  .ne. 2) error stop 4_4
         if (u2(1)%rid .ne. 3) error stop 5_4
         if (u2(2)%rid .ne. 3) error stop 6_4
      class default
         error stop 7_4
   end select

   select type (u3)
      class is (child(4,4))
         if (u3%id .ne. 2) error stop 8_4
         if (u3%rid .ne. 3) error stop 9_4
      class default
         error stop 10_4
   end select

end program