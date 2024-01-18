!****************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign065.f
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

  type(container) :: c1,c2
  type(base), target :: tar1(50)=(/(base(i),i=1,50)/)
  integer :: lowerb, upperb

  lowerb=25
  upperb=74

  associate(x=>c1%b2,y=>c2%b2)

    x%ptr(lowerb:upperb)=>tar1

    if(lbound(x%ptr, dim=1).ne. 25) error stop 1
    if(ubound(x%ptr, dim=1).ne. 74) error stop 2
    if(any(shape(x%ptr).ne.(/50/))) error stop 3
    if(.not.associated(x%ptr,tar1)) error stop 4

   y%ptr(10:59)=>tar1

    if(lbound(y%ptr, dim=1).ne. 10) error stop 5
    if(ubound(y%ptr, dim=1).ne. 59) error stop 6
    if(any(shape(y%ptr).ne.(/50/))) error stop 7
    if(.not.associated(y%ptr,tar1)) error stop 8

  end associate

    if(lbound(c1%b2%ptr, dim=1).ne. 25) error stop 9
    if(ubound(c1%b2%ptr, dim=1).ne. 74) error stop 10
    if(any(shape(c1%b2%ptr).ne.(/50/))) error stop 11
    if(.not.associated(c1%b2%ptr,tar1)) error stop 12

    if(lbound(c2%b2%ptr, dim=1).ne. 10) error stop 13
    if(ubound(c2%b2%ptr, dim=1).ne. 59) error stop 14
    if(any(shape(c2%b2%ptr).ne.(/50/))) error stop 15
    if(.not.associated(c2%b2%ptr,tar1)) error stop 16

end program


