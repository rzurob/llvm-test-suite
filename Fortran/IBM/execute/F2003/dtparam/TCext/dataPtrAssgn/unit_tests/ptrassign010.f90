! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign010.f
! opt variations: -ql -qreuse=none

!****************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign068.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 31, 2006
!*  ORIGIN                     : AIX Compiler Development,
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

  type base(k1)    ! (4)
    integer, kind :: k1
    integer(k1)   :: data
  end type

  type, extends(base) :: container    ! (4)
    integer(k1) :: more_data
  end type

end module

use m
class(base(4)), pointer :: ptr(:), ptr2(:,:), ptr3
type(container(4)), target :: tar1(30)
integer :: num=1

do i=1,30
    tar1(i)=container(4)(i,i)
end do

ptr=>tar1

select type(ptr)

  type is (base(4))
    error stop 7
  type is (container(4))
    ptr2(11:20,4:6)=>ptr

    if(lbound(ptr2, dim=1).ne. 11) error stop 1
    if(lbound(ptr2, dim=2).ne. 4) error stop 2
    if(ubound(ptr2, dim=1).ne. 20) error stop 3
    if(ubound(ptr2, dim=2).ne. 6) error stop 4
    if(any(shape(ptr2).ne.(/10,3/))) error stop 5

    do i=4,6
      do j=11,20
        ptr3=>ptr2(j,i)
        if(.not.associated(ptr3,tar1(num))) error stop 6
        num=num+1
      end do
    end do
    num=1

    class default
      error stop 8
end select

    if(lbound(ptr2, dim=1).ne. 11) error stop 7
    if(lbound(ptr2, dim=2).ne. 4) error stop 8
    if(ubound(ptr2, dim=1).ne. 20) error stop 9
    if(ubound(ptr2, dim=2).ne. 6) error stop 10
    if(any(shape(ptr2).ne.(/10,3/))) error stop 11

    do i=4,6
      do j=11,20
        ptr3=>ptr2(j,i)
        if(.not.associated(ptr3,tar1(num))) error stop 12
        num=num+1
      end do
    end do




end
