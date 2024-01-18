!****************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign063.f
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

    type base
      integer :: data
    end type

    type base2
      type(base), pointer :: ptr(:)
    end type

    type container
      type(base2) :: b2
    end type

  end module

  use m

  type(container) :: c1
  type(base), target :: tar(50)=(/(base(i),i=1,50)/)
  integer :: lowerb, upperb

  lowerb=25
  upperb=sum(ubound(tar))-1
  c1%b2%ptr(lowerb:upperb)=>tar

  associate(x=>c1%b2)

    if(lbound(x%ptr, dim=1).ne. 25) error stop 1
    if(ubound(x%ptr, dim=1).ne. 49) error stop 2
    if(any(shape(x%ptr).ne.(/25/))) error stop 3
    if(.not.associated(x%ptr,tar(1:25))) error stop 4

  end associate

    if(lbound(c1%b2%ptr, dim=1).ne. 25) error stop 5
    if(ubound(c1%b2%ptr, dim=1).ne. 49) error stop 6
    if(any(shape(c1%b2%ptr).ne.(/25/))) error stop 7
    if(.not.associated(c1%b2%ptr,tar(1:25))) error stop 8

end program


