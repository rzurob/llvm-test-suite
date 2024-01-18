! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign018.f
! opt variations: -ql

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
!*  DESCRIPTION                :functional testing of bounds-remapping
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  type dt(k1)    ! (4)
    integer, kind :: k1
    integer(k1)   :: num
  end type

  integer :: num1=1

  type(dt(4)), pointer :: ptr1(:,:)
  type(dt(4)), pointer :: ptr2(:), ptr3
  type(dt(4)), target  :: tar1(1:20)=(/(dt(4)(i), i=1,20)/)

  ptr2(5:24)=>tar1

  if(lbound(ptr2, dim=1).ne. 5) error stop 1
  if(ubound(ptr2, dim=1).ne. 24) error stop 2
  if(any(shape(ptr2).ne.(/20/))) error stop 3
  if(.not.associated(ptr2,tar1)) error stop 4

  ptr1(10:14,15:18)=>ptr2

  if(lbound(ptr1, dim=1).ne. 10) error stop 5
  if(lbound(ptr1, dim=2).ne. 15) error stop 6
  if(ubound(ptr1, dim=1).ne. 14) error stop 7
  if(ubound(ptr1, dim=2).ne. 18) error stop 8
  if(any(shape(ptr1).ne.(/5,4/))) error stop 9

  do i=15,18
    do j=10,14
      ptr3=>ptr1(j,i)
      if(.not.associated(ptr3,tar1(num1))) error stop 11
      num1=num1+1
    end do
  end do

end




