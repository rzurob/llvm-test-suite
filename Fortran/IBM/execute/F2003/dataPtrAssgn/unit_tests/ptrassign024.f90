!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign024.f
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

  type base
    integer :: num1
  end type

  type ,extends(base) :: child
    integer :: num2
  end type

  integer :: num=1

  class(base), target, allocatable :: tar1(:)

  type(base), pointer :: ptr1(:,:), ptr2

  allocate(tar1(10), source=(/(child(i,i),i=1,10)/))

  ptr1(3:5,6:8)=>tar1

  if(lbound(ptr1, dim=1).ne. 3) error stop 1
  if(lbound(ptr1, dim=2).ne. 6) error stop 2
  if(ubound(ptr1, dim=1).ne. 5) error stop 3
  if(ubound(ptr1, dim=2).ne. 8) error stop 4
  if(any(shape(ptr1).ne.(/3,3/))) error stop 5

  select type(tar1)
  type is(child)
  do i=6,8
    do j=3,5
      ptr2=>ptr1(j,i)
      if(.not.associated(ptr2,tar1(num)%base)) error stop 6
      num=num+1
    end do
  end do
  class default
    error stop 7
  end select



end

