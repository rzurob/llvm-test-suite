! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/allocate/alloc003.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Allocate statement - type-spec cannot be non-poly abstract type
!*                                        abstract polymorphic entity to be allocated with extension of abstract type
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

   type, extends(base) :: child(k2,n1)    ! (4,4,20)
       integer, kind :: k2
       integer, len  :: n1
   end type

end module

program alloc003
   use m
   class(base(4)), allocatable    :: b1
   class(base(4)), allocatable, dimension(:) :: b2

   class(child(4,4,20)), allocatable :: c1
   type(child(4,4,20)) :: c2 = child(4,4,20)(1)

   allocate(c1, source= child(4,4,20)(1) )

   allocate( b1, source = c1 )
   allocate( b2(2), source = (/c1,c2/) )

   if (( b1%id .ne. 1 ))  error stop 1_4
   if (( b2(1)%id .ne. 1 ) .or. ( b2(2)%id .ne. 1 )) error stop 2_4

end program
