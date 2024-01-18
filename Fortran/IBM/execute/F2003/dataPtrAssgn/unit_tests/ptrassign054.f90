!****************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptr1assign054.f
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

  complex, pointer :: ptr1(:,:), ptr2(:,:), ptr3
  complex, target  :: tar1(100), tar2(10)
  integer :: num=1

  volatile :: ptr1, tar1

  do i=1,100
    tar1(i)=cmplx(i,i)
  end do

  do i=20,29
    tar2(i)=cmplx(i,i)
  end do

  !volatile pointer and target
  ptr1(5:9,10:14)=>tar1

  if(lbound(ptr1, dim=1).ne. 5) error stop 1
  if(lbound(ptr1, dim=2).ne. 10) error stop 2
  if(ubound(ptr1, dim=1).ne. 9) error stop 3
  if(ubound(ptr1, dim=2).ne. 14) error stop 4
  if(any(shape(ptr1).ne.(/5,5/))) error stop 5

  do i=10,14
    do j=5,9
      ptr3=>ptr1(j,i)
      if(.not.associated(ptr3,tar1(num))) error stop 6
      num=num+1
    end do
  end do
  num=1

  !volatile pointer
  ptr1(5:9,5:6)=>tar2

  if(lbound(ptr1, dim=1).ne. 5) error stop 7
  if(lbound(ptr1, dim=2).ne. 5) error stop 8
  if(ubound(ptr1, dim=1).ne. 9) error stop 9
  if(ubound(ptr1, dim=2).ne. 6) error stop 10
  if(any(shape(ptr1).ne.(/5,2/))) error stop 11

  do i=5,6
    do j=5,9
      ptr3=>ptr1(j,i)
      if(.not.associated(ptr3,tar2(num))) error stop 12
      num=num+1
    end do
  end do
  num=1

  !volatile target
  ptr2(25:34,30:34)=>tar1

  if(lbound(ptr2, dim=1).ne. 25) error stop 13
  if(lbound(ptr2, dim=2).ne. 30) error stop 14
  if(ubound(ptr2, dim=1).ne. 34) error stop 15
  if(ubound(ptr2, dim=2).ne. 34) error stop 16
  if(any(shape(ptr2).ne.(/10,5/))) error stop 17

  do i=30,34
    do j=25,34
      ptr3=>ptr2(j,i)
      if(.not.associated(ptr3,tar1(num))) error stop 18
      num=num+1
    end do
  end do
  num=1

  !nothing volatile
  ptr2(10:19,1:1)=>tar2

  if(lbound(ptr2, dim=1).ne. 10) error stop 19
  if(lbound(ptr2, dim=2).ne. 1) error stop 20
  if(ubound(ptr2, dim=1).ne. 19) error stop 21
  if(ubound(ptr2, dim=2).ne. 1) error stop 22
  if(any(shape(ptr2).ne.(/10,1/))) error stop 23

  do i=1,1
    do j=10,19
      ptr3=>ptr2(j,i)
      if(.not.associated(ptr3,tar2(num))) error stop 24
      num=num+1
    end do
  end do

end
