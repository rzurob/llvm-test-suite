! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/selectType/selectType008.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Select Type Construct with array
!*                               TYPE is (abstract type)
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
      integer(k1)   :: i = 5
   end type

   type, extends(base) :: child(k2,n1)    ! (4,4,20)
       integer, kind :: k2
       integer, len  :: n1
   end type

end module

program selectType008
   use m

   class(base(4)), allocatable :: b1(:)
   allocate (b1(2), source = (/ child(4,4,20)(), child(4,4,20)() /))
   select type ( b => b1 )
      type is (base(4))
         print *, 'error'
   end select

end program
