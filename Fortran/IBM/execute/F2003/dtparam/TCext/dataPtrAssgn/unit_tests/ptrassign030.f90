! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/dataPtrAssgn/unit_tests/ptrassign030.f
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

  class(base(:,4)), allocatable, target :: tar1(:,:,:)

  class(base(:,4)), pointer :: ptr(:,:,:)

  allocate(base(20,4) :: tar1(3,4,5))


  ptr(0:,-1:, -2: )=>tar1

  select type (ptr)
    type is (base(*,4))

     if(lbound(ptr, dim=1).ne. 0) error stop 1
     if(lbound(ptr, dim=2).ne. -1) error stop 2
     if(lbound(ptr, dim=3).ne. -2) error stop 3
     if(ubound(ptr, dim=1).ne. 2) error stop 4
     if(ubound(ptr, dim=2).ne. 2) error stop 5
     if(ubound(ptr, dim=3).ne. 2) error stop 6
     if(any(shape(ptr).ne.(/3,4,5/))) error stop 7


   class default
     error stop 9
  end select

  if(.not.associated(ptr,tar1)) error stop 10


end

