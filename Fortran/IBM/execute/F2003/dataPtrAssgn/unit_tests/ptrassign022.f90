!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: ptrassign022.f
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

  integer, pointer :: ptr1(:,:), ptr2(:), ptr3

  integer :: lbound1, lbound2,num1

  integer, target :: tar1(25)= (/(i,i=1,25)/)

  lbound1=4
  lbound2=8
  num1=8

  ptr1(lbound1:lbound1+3,lbound2:lbound2+3)=>tar1(8:)

  if(lbound(ptr1, dim=1).ne. 4) error stop 1
  if(lbound(ptr1, dim=2).ne. 8) error stop 2
  if(ubound(ptr1, dim=1).ne. 7) error stop 3
  if(ubound(ptr1, dim=2).ne. 11) error stop 4
  if(any(shape(ptr1).ne.(/4,4/))) error stop 5
  if(loc(ptr1).ne.loc(tar1(8))) error stop 6

  do i=8,11
    do j=4,7
      ptr3=>ptr1(j,i)
      if(.not.associated(ptr3,tar1(num1))) error stop 7
      num1=num1+1
    end do
  end do

  ptr2(lbound2-lbound1:lbound2+lbound1)=>tar1(12:)

  if(lbound(ptr2, dim=1).ne. 4) error stop 8
  if(ubound(ptr2, dim=1).ne. 12) error stop 9
  if(any(shape(ptr2).ne.(/9/))) error stop 10

  num1=12
  do i=4,12
    ptr3=>ptr2(i)
    if(.not.associated(ptr3,tar1(num1))) error stop 11
    num1=num1+1
  end do

end

