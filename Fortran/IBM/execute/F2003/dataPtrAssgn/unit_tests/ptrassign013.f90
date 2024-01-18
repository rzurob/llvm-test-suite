!****************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign067.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : ptrassign067
!*
!*  PROGRAMMER                 : Michael Selvanayagam
!*  DATE                       : March 31, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf2003
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                :functional testing of bounds-remapping and bounds-spec
!*                              
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

  type base
    integer :: data
  end type
  
  type, extends(base) :: container
    integer :: more_data
  end type

end module

use m
class(base), pointer :: ptr(:,:), ptr2(:,:)
type(container), target :: tar1(6,5)

do i=1,5
  do j=1,6
    tar1(j,i)=container(j,i)
  end do
end do

ptr=>tar1

select type(ptr)

  type is (base)
    error stop 7
  type is (container)
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