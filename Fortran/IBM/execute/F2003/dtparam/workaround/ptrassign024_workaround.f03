! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign024.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

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

  type base(n1,k1)    ! (20,4)
    integer, kind :: k1
    integer, len  :: n1
    integer(k1)   :: num1
  end type

  type ,extends(base) :: child    ! (20,4)
    integer(k1) :: num2
  end type

  integer :: num=1

  class(base(:,4)), target, allocatable :: tar1(:)

  type(base(:,4)), pointer :: ptr1(:,:), ptr2

  allocate(tar1(10), source=(/(child(20,4)(i,i),i=1,10)/))

  ptr1(3:5,6:8)=>tar1

  if(lbound(ptr1, dim=1).ne. 3) error stop 1
  if(lbound(ptr1, dim=2).ne. 6) error stop 2
  if(ubound(ptr1, dim=1).ne. 5) error stop 3
  if(ubound(ptr1, dim=2).ne. 8) error stop 4
  if(any(shape(ptr1).ne.(/3,3/))) error stop 5

  select type(tar1)
  type is(child(*,4))
  do i=6,8
    do j=3,5
      ptr2=>ptr1(j,i)
!      if(.not.associated(ptr2,tar1(num)%base)) error stop 6
        if (ptr1(j,i)%num1 /= num) error stop 7
        if (ptr2%num1 /= num) error stop 8
      num=num+1
    end do
  end do
  class default
    error stop 7
  end select



end

