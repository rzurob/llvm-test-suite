! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=none /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign029.f
! opt variations: -qnol -qnodeferredlp -qreuse=base

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

  type ,extends(base) :: child(n2,k2)    ! (20,4,20,4)
    integer, kind :: k2
    integer, len  :: n2
    integer(k2)   :: num2
  end type

  class(base(:,4)), target, allocatable :: tar1(:,:)

  type(base(:,4)), pointer :: ptr1(:,:)

  allocate(base(20,4) :: tar1(10,10))

  ptr1(5:,10:)=>tar1

  if(lbound(ptr1, dim=1).ne. 5) error stop 1
  if(lbound(ptr1, dim=2).ne. 10) error stop 2
  if(ubound(ptr1, dim=1).ne. 14) error stop 3
  if(ubound(ptr1, dim=2).ne. 19) error stop 4
  if(any(shape(ptr1).ne.(/10,10/))) error stop 5

  select type(tar1)
  type is(base(*,4))
      if(.not.associated(ptr1,tar1)) error stop 6
  class default
    error stop 7
  end select

 end

