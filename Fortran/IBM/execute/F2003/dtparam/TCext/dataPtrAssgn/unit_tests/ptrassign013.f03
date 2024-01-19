! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign013.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!****************************************************************
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                :functional testing of bounds-remapping and bounds-spec
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

  type base(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    integer(k1)   :: data
  end type

  type, extends(base) :: container    ! (20,4)
    integer(k1) :: more_data
  end type

end module

use m
class(base(:,4)), pointer :: ptr(:,:), ptr2(:,:)
type(container(20,4)), target :: tar1(6,5)

do i=1,5
  do j=1,6
    tar1(j,i)=container(20,4)(j,i)
  end do
end do

ptr=>tar1

select type(ptr)

  type is (base(*,4))
    error stop 7
  type is (container(*,4))
    ptr2(15:,20:)=>ptr

    if(lbound(ptr2, dim=1).ne. 15) error stop 1
    if(lbound(ptr2, dim=2).ne. 20) error stop 2
    if(ubound(ptr2, dim=1).ne. 20) error stop 3
    if(ubound(ptr2, dim=2).ne. 24) error stop 4
    if(any(shape(ptr2).ne.(/6,5/))) error stop 5
    if(.not.associated(ptr2,tar1)) error stop 6
  class default
      error stop 8
end select

    if(lbound(ptr2, dim=1).ne. 15) error stop 7
    if(lbound(ptr2, dim=2).ne. 20) error stop 8
    if(ubound(ptr2, dim=1).ne. 20) error stop 9
    if(ubound(ptr2, dim=2).ne. 24) error stop 10
    if(any(shape(ptr2).ne.(/6,5/))) error stop 11
    if(.not.associated(ptr2,tar1)) error stop 12



end
