!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign017.f
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
!*  DESCRIPTION                :functional testing of bounds-remapping
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  type dtptr
    real, pointer :: ptr1(:,:)
    real, pointer :: ptr2(:)
  end type

  type dttar
    real :: tar1(25)=(/(real(i),i=1,25)/)
  end type

  integer :: num1=1

  type(dtptr) :: dtptr1
  type(dttar), target :: dttar1

  real, pointer :: ptr3

  dtptr1%ptr1(5:9,10:14)=>dttar1%tar1

  if(lbound(dtptr1%ptr1,	dim=1).ne. 5) error stop 1
  if(lbound(dtptr1%ptr1, dim=2).ne. 10) error stop 2
  if(ubound(dtptr1%ptr1, dim=1).ne. 9) error stop 3
  if(ubound(dtptr1%ptr1, dim=2).ne. 14) error stop 4
  if(any(shape(dtptr1%ptr1).ne.(/5,5/))) error stop 5
  !if(loc(dtptr1%ptr1).ne.loc(dttar1%tar1)) error stop 6


  do i=10,14
    do j=5,9
      ptr3=>dtptr1%ptr1(j,i)
      if(.not.associated(ptr3,dttar1%tar1(num1))) error stop 7
      num1=num1+1
    end do
  end do

  dtptr1%ptr2(15:39)=>dttar1%tar1

  if(lbound(dtptr1%ptr2,	dim=1).ne. 15) error stop 8
  if(ubound(dtptr1%ptr2, dim=1).ne. 39) error stop 9
  if(any(shape(dtptr1%ptr2).ne.(/25/))) error stop 10

  num1=1
  do i=15,39
    ptr3=>dtptr1%ptr2(i)
    if(.not.associated(ptr3,dttar1%tar1(num1))) error stop 12
    num1=num1+1
  end do

end
